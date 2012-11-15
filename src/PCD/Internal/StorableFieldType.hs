{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module PCD.Internal.StorableFieldType (parseBinaryPoints, pokeBinaryPoints) where
import Control.Applicative
import Control.Lens ((^.))
import Control.Monad (void)
import qualified Data.Vector as B
import qualified Data.Vector.Mutable as BM
import PCD.Header
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import System.IO (Handle, hGetBuf)

-- Strict tuple used during parsing.
data P a = P !FieldType {-# UNPACK #-} !(Ptr a)

-- |'peek' a 'Storable' and advance the source pointer past this
-- datum.
peekStep :: forall a b. Storable a => (a -> FieldType) -> Ptr b -> IO (P b)
peekStep mk ptr = P . mk <$> peek (castPtr ptr) 
                         <*> pure (plusPtr ptr (sizeOf (undefined::a)))

parseBinaryField :: DimType -> Int -> Ptr a -> IO (P a)
parseBinaryField I 1 = peekStep TChar
parseBinaryField I 2 = peekStep TShort
parseBinaryField I 4 = peekStep TInt
parseBinaryField U 1 = peekStep TUchar
parseBinaryField U 2 = peekStep TUshort
parseBinaryField U 4 = peekStep TUint
parseBinaryField F 4 = peekStep TFloat
parseBinaryField F 8 = peekStep TDouble
parseBinaryField t s = error ("Unknown field type: "++show t++" "++show s)

parseBinaryPoints :: Header -> Handle -> IO (B.Vector (B.Vector FieldType))
parseBinaryPoints pcdh h = 
  B.unsafeFreeze =<<
  do v <- BM.new n
     let go !i !ptr
           | i == n = return v
           | otherwise = do (pt,ptr') <- pointParserBin ptr
                            BM.write v i pt
                            go (i+1) ptr'
     allocaBytes numBytes $ \ptr -> 
       hGetBuf h ptr numBytes >>
       go 0 ptr
  where n = fromIntegral $ pcdh ^. points
        numBytes = n * sum (zipWith (*) (pcdh^.counts) (pcdh^.sizes))
        pointParserBin = parseBinaryFields pcdh

-- Parse all fields of a single point. A point is represented as a
-- 'B.Vector' of its fields. The returned 'Ptr' is just after the
-- parsed point.
parseBinaryFields :: Header -> Ptr a -> IO (B.Vector FieldType, Ptr a)
parseBinaryFields h = aux
  where numFields = sum (h^.counts)
        aux ptr0 = (\(v,ptr) -> (,) <$> B.unsafeFreeze v <*> pure ptr) =<<
                   do v <- BM.new numFields
                      let write = BM.write v
                          go !i !ptr ss ts cs
                            | i == numFields = return (v,ptr)
                            | otherwise = 
                              do P x ptr' <- parseBinaryField (head ts) 
                                                              (head ss)
                                                              ptr
                                 write i x
                                 let (c:cs') = cs
                                 if c == 1
                                   then go (i+1) ptr' (tail ss) (tail ts) cs'
                                   else go (i+1) ptr' ss ts (c-1 : cs')
                      go 0 ptr0 (h^.sizes) (h^.dimTypes) (h^.counts)

pokeStep :: forall a b. Storable a => a -> Ptr b -> IO (Ptr b)
pokeStep x ptr = poke (castPtr ptr) x >>
                 return (plusPtr ptr (sizeOf (undefined::a)))

pokeBinaryField :: FieldType -> Ptr a -> IO (Ptr a)
pokeBinaryField (TUchar x) = pokeStep x
pokeBinaryField (TChar x) = pokeStep x
pokeBinaryField (TUshort x) = pokeStep x
pokeBinaryField (TShort x) = pokeStep x
pokeBinaryField (TUint x) = pokeStep x
pokeBinaryField (TInt x) = pokeStep x
pokeBinaryField (TFloat x) = pokeStep x
pokeBinaryField (TDouble x) = pokeStep x

pokeBinaryFields :: Ptr a -> B.Vector FieldType -> IO (Ptr a)
pokeBinaryFields = B.foldM' aux
  where aux ptr x = pokeBinaryField x ptr

pokeBinaryPoints :: Ptr a -> B.Vector (B.Vector FieldType) -> IO ()
pokeBinaryPoints = (void .) . B.foldM' pokeBinaryFields