{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts, 
             BangPatterns #-}
-- |Define a data structure for a PCD file header and an associated
-- parser.
module PCD.Header where
import Control.Applicative
import Control.Arrow ((***))
import Control.Monad.State
import Data.Foldable (Foldable, foldMap)
import Data.Int
import Data.List (intersperse)
import Data.Monoid (mconcat, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Data.Word
import Data.Attoparsec.Text hiding (I)
import System.IO (Handle)
import Control.DeepSeq
import PCD.Internal.SmallLens
import PCD.Internal.Types

-- |Fields attached to a point may be signed integers (I), unsigned
-- integers (U), or floating point (F).
data DimType = I | U | F deriving (Eq,Show,Ord)

-- |The PCD format has both ASCII and binary variants.
data DataFormat = ASCII | Binary deriving (Eq,Show,Ord)

data FieldType = TUchar  {-# UNPACK #-}!Word8
               | TChar   {-# UNPACK #-}!Int8
               | TUshort {-# UNPACK #-}!Word16
               | TShort  {-# UNPACK #-}!Int16
               | TUint   {-# UNPACK #-}!Word32
               | TInt    {-# UNPACK #-}!Int32
               | TFloat  {-# UNPACK #-}!Float
               | TDouble {-# UNPACK #-}!Double
                 deriving Show

class PCDType a where
  -- |Extract a raw Haskell value from the 'FieldType' variant. If you
  -- know what you've got, this frees from having to pattern match on
  -- the 'FieldType' constructor. If you're wrong, you'll get an
  -- exception.
  unsafeUnwrap :: FieldType -> a
  -- |Associate a 'DimType' and a size (in bytes) for every instance
  -- of 'PCDType'. The argument to 'fromHaskell' is never evaluated.
  fromHaskell :: a -> (DimType,Int)

instance PCDType Word8 where
  unsafeUnwrap (TUchar x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Word8"
  fromHaskell _ = (U,1)

instance PCDType Int8 where
  unsafeUnwrap (TChar x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Int8"
  fromHaskell _ = (I,1)

instance PCDType Word16 where
  unsafeUnwrap (TUshort x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Word16"
  fromHaskell _ = (U,2)

instance PCDType Int16 where
  unsafeUnwrap (TShort x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Int16"
  fromHaskell _ = (I,2)

instance PCDType Word32 where
  unsafeUnwrap (TUint x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Word32"
  fromHaskell _ = (U,4)

instance PCDType Int32 where
  unsafeUnwrap (TInt x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Int32"
  fromHaskell _ = (I,4)

instance PCDType Float where
  unsafeUnwrap (TFloat x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Float"
  fromHaskell _ = (F,4)

instance PCDType Double where
  unsafeUnwrap (TDouble x) = x
  unsafeUnwrap y = error $ "Tried to unwrap "++show y++" as a Double"
  fromHaskell _ = (F,8)

-- |Construct a parser for a field based on its type and size.
fieldParser :: DimType -> Int -> Parser FieldType
fieldParser I 1 = TChar <$> (decimal :: Parser Int8)
fieldParser I 2 = TShort <$> (decimal :: Parser Int16)
fieldParser I 4 = TInt <$> (decimal :: Parser Int32)
fieldParser U 1 = TUchar <$> (decimal :: Parser Word8)
fieldParser U 2 = TUshort <$> (decimal :: Parser Word16)
fieldParser U 4 = TUint <$> (decimal :: Parser Word32)
fieldParser F 4 = TFloat . realToFrac <$> double
fieldParser F 8 = TDouble <$> double
fieldParser t s = error $ "Unknown field description: "++show t++" "++show s

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do !x' <- x
                      !xs' <- sequence' xs
                      return $ x':xs'

data Header = Header { _version   :: Text 
                     , _fields    :: [Text]
                     , _sizes     :: [Int]
                     , _dimTypes  :: [DimType]
                     , _counts    :: [Int]
                     , _width     :: Integer
                     , _height    :: Int
                     , _viewpoint :: (V3 Double, Quaternion Double)
                     , _points    :: Integer
                     , _format    :: DataFormat } deriving Show
makeLenses ''Header

-- |The default PCD version of 0.7.
defaultVersion :: Text
defaultVersion = "0.7"

-- |Make a PCD header for a monotyped vector point
-- type. @mkSimpleHeader fields (type,sz) n@ prepares a 'Header' for
-- @n@ points with field names @fields@, field type given by @type@,
-- and field size given by @sz@. Example to save 1000 3D points:
--
-- > mkSimpleHeader ["x","y","z"] (F,4) 1000
mkSimpleHeader :: [Text] -> (DimType,Int) -> Int -> Header
mkSimpleHeader fields (t,sz) numPts = 
  Header "0.7" fields (rep sz) (rep t) (rep 1) n 1 (0, Quaternion 1 0) n Binary
    where numFields = length fields
          rep = replicate numFields
          n = fromIntegral numPts

-- |@mkHeaderXYZ sample n@ builds a 'Header' for @n@ points with
-- fields \"x\", \"y\", and \"z\" of 'DimType' and size (in bytes)
-- derived from the 'PCDType' instance of @sample@. Example:
-- 
-- > mkHeaderXYZ (undefined::Float) 1000
mkHeaderXYZ :: PCDType a => a -> Int -> Header
mkHeaderXYZ = mkSimpleHeader ["x","y","z"] . fromHaskell

-- |Assemble a parser for points by sequencing together all necessary
-- field parsers.
pointParser :: Header -> Parser [FieldType]
pointParser h = sequence' . map (<* skipSpace) $ 
                zipWith fieldParser (_dimTypes h) (_sizes h)

-- |Create a 'Header' based on an existing one that keeps only the
-- fields whose names pass the supplied predicate.
filterFields :: (Text -> Bool) -> Header -> Header
filterFields f h = h % (fields %~ keep) . (sizes %~ keep)
                     . (dimTypes %~ keep) . (counts %~ keep)
  where keepers = map f (_fields h)
        keep = map snd . filter fst . zip keepers

instance NFData Header where
  rnf (Header !_v !_f !_s !_d !_c !_w !_h !(!_t,!_r) !_p !_fmt) = ()

defaultHeader :: Header
defaultHeader = Header "" [] [] [] [] 0 0 (0, Quaternion 1 0) 0 ASCII

readVersion :: Parser Text
readVersion = "VERSION" .*> space *> takeText

readFields :: Parser [Text]
readFields = "FIELDS" .*> space *> fmap T.words takeText

readTypes :: Parser [DimType]
readTypes = "TYPE" .*> space *> sepBy t space
  where t = "I" .*> return I <|> "U" .*> return U <|> "F" .*> return F

namedIntegral :: Integral a => Text -> Parser a
namedIntegral n = n .*> space *> decimal

namedIntegrals :: Integral a => Text -> Parser [a]
namedIntegrals n = n .*> space *> sepBy decimal space

readViewpoint :: Parser (V3 Double, Quaternion Double)
readViewpoint = "VIEWPOINT" .*> space *> ((,) <$> v <*> q)
  where v = fmap (\[x,y,z] -> V3 x y z) $ count 3 (double <* skipSpace)
        q = fmap (\[w,i,j,k] -> Quaternion w (V3 i j k)) $ 
            count 4 (double <* skipSpace)

readFormat :: Parser DataFormat
readFormat = "DATA" .*> space *> 
             (("ascii" .*> pure ASCII) <|> ("binary" .*> pure Binary))

-- |Get the next non-comment line.
nextLine :: Handle -> IO Text
nextLine h = do t <- hGetLine h 
                either (const $ return t) (const $ nextLine h) $ 
                  parseOnly isComment t
  where isComment = string "#"

-- |Parse a PCD header. Returns the 'Header' and the rest of the file
-- contents.
readHeader :: Handle -> IO (Header, Maybe Text)
readHeader h = flip execStateT (defaultHeader, Nothing) $ 
                  sequence_ [ entry readVersion version
                            , entry readFields fields
                            , entry (namedIntegrals "SIZE") sizes
                            , entry readTypes dimTypes
                            , entry (namedIntegrals "COUNT") counts
                            , entry (namedIntegral "WIDTH") width
                            , entry (namedIntegral "HEIGHT") height
                            , entry readViewpoint viewpoint 
                            , entry (namedIntegral "POINTS") points
                            , entry readFormat format ]
  where nxt :: (MonadState (a,Maybe Text) m, MonadIO m) => m ()
        nxt = liftIO (nextLine h) >>= (_2.=) . Just
        entry :: (MonadState (s,Maybe Text) m, MonadIO m, Functor m) => 
                 Parser a -> Setting s s a a -> m ()
        entry parser field = do use _2 >>= maybe nxt (const (return ()))
                                Just ln <- use _2
                                case parseOnly parser ln of
                                  Left _ -> return ()
                                  Right v -> _1 . field .= v >> _2.=Nothing

-- |Format a 'Header' to be compatible with the PCD specification.
writeHeader :: Header -> Text
writeHeader h = (<> "\n") . mconcat . intersperse "\n" $
                [ "VERSION " <> (h^.version)
                , "FIELDS " <> joinFields (h^.fields)
                , "SIZE " <> joinFields (map tshow (h^.sizes))
                , "TYPE " <> joinFields (map tshow (h^.dimTypes))
                , "COUNT " <> joinFields (map tshow (h^.counts))
                , "WIDTH " <> tshow (h^.width)
                , "HEIGHT " <> tshow (h^.height)
                , "VIEWPOINT " <> (uncurry (<>) . (ftshow***((" "<>).ftshow)) $
                                   (h^.viewpoint))
                , "POINTS " <> tshow (h^.points)
                , "DATA " <> T.toLower (tshow (h^.format)) ]
  where joinFields = mconcat . intersperse " "
        tshow :: Show a => a -> Text
        tshow = T.pack . show
        ftshow :: (Foldable t, Show a) => t a -> Text
        ftshow = mconcat . intersperse " " . foldMap ((:[]) . tshow)

-- |Compute the number of bytes this point cloud would occupy if
-- serialized with the 'Binary' encoding.
totalBinarySize :: Header -> Int
totalBinarySize h = fromIntegral (h^.points) * 
                    sum (zipWith (*) (h^.counts) (h^.sizes))
