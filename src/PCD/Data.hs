{-# LANGUAGE ScopedTypeVariables #-}
-- |Parser for PCD (point cloud data) files.
module PCD.Data (-- * Accessing fields
                 FieldType(..), unsafeUnwrap, 
                 -- * Loading PCD data
                 loadFieldsByName, loadFlexiblePoints, loadXyzw, loadXyz, 
                 -- * Saving PCD data
                 saveBinaryPcd, projectBinaryFields,
                 -- * PCD header creation
                 mkSimpleHeader, mkHeaderXYZ) where
import Control.Applicative
import Control.DeepSeq
import Control.Lens ((.~), (^.))
import qualified Data.Attoparsec.Text.Lazy as ATL
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Vector as B
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable, sizeOf)
import System.IO (Handle, openFile, hClose, 
                  IOMode(..), withBinaryFile, hPutBuf, hGetBuf)
import PCD.Header
import qualified PCD.Internal.AsciiParsers as A
import PCD.Internal.StorableFieldType
import PCD.Internal.Types

-- |Read back 'Storable' points saved as binary data.
readStorableBinaryPoints :: forall a. Storable a => 
                              Header -> Handle -> IO (Either String (Vector a))
readStorableBinaryPoints pcd h
  | ptSize /= sz = return . Left $ 
                   "Deserialization type is not the same size as the points "++
                   "described by this file. The PCD file dicates "++
                   show ptSize++" bytes per point; destination type takes up "++
                   show sz++" bytes."
  | otherwise  = do vm <- VM.new (fromIntegral $ pcd^.points)
                    _ <- VM.unsafeWith vm (flip (hGetBuf h) numBytes)
                    Right <$> V.freeze vm
  where sz = sizeOf (undefined::a)
        numBytes = fromIntegral (pcd^.points) * sz
        ptSize = sum (_sizes pcd)

-- |Reads point data in either ASCII or binary formats given an ASCII
-- parser for the point data type and a 'Storable' instance. If you
-- know that your points are binary or ASCII, consider using
-- 'readBinPoints' or 'readAsciiPoints'.
readPointData :: Storable a => 
                 Header -> Handle -> ATL.Parser a -> 
                 IO (Either String (Vector a))
readPointData header handle parser
  | header^.format == ASCII = Right <$> A.readPoints header handle parser
  | otherwise = readStorableBinaryPoints header handle

-- |Use an existing PCD header to save binary point data to a
-- file. The supplied header is used as-is, except that its format is
-- set to 'Binary'.
saveBinaryPcd :: forall a. Storable a => 
                 FilePath -> Header -> V.Vector a -> IO ()
saveBinaryPcd outputFile pcd pts = 
  do putStrLn $ "Converting "++show (V.length pts)++" points"
     let pcd' = format .~ Binary $ pcd
         sz = sizeOf (undefined::a) * V.length pts
     T.writeFile outputFile (writeHeader pcd')
     withBinaryFile outputFile AppendMode $ \h ->
       V.unsafeWith pts (flip (hPutBuf h) sz)

-- |Save a binary PCD file including only the named fields. This is
-- useful when you have a PCD file that has more fields for each point
-- than you care about. For instance, you may wish to extract just the
-- \"x\", \"y\", and \"z\" fields for each point. This can be
-- accomplished using, @projectBinaryFields [\"x\", \"y\", \"z\"]
-- inputFile outputFile@.
projectBinaryFields :: [Text] -> FilePath -> FilePath -> IO ()
projectBinaryFields fs i o = 
  do h <- openFile i ReadMode
     (pcdh,_) <- readHeader h
     v <- loadFlexiblePoints pcdh h
     putStrLn $ "Parsed "++show (B.length v)++" ASCII points"
     let v' = B.map keep v
         keepers = B.fromList $ map (`elem` fs) (pcdh ^. fields)
         keep = B.map snd . B.filter fst . B.zip keepers
         pcdh' = format .~ Binary $ filterFields (`elem` fs) pcdh
         numBytes = totalBinarySize pcdh'
     putStrLn $ "Binary data will occupy "++show numBytes++" bytes"
     hClose h
     T.writeFile o (writeHeader pcdh')
     withBinaryFile o AppendMode $ \h' ->
       allocaBytes numBytes $ \ptr ->
         pokeBinaryPoints ptr v' >>
         hPutBuf h' ptr numBytes
     return ()

-- |Load points stored in a PCD file into a 'Vector'. This requires a
-- 'Storable' instance for the type used to represent a point. If the
-- point is a monotyped collection of fields, consider using
-- 'Linear.V2', 'Linear.V3', or 'Linear.V4' to represent points. When
-- using a representation from the @linear@ package, you may wish to
-- use 'loadXyz' or 'loadXyzw' which can handle ASCII and Binary
-- serializations of 3D or 4D points.
loadPoints :: Storable a => ATL.Parser a -> FilePath -> IO (Vector a)
loadPoints parser pcdFile = do h <- openFile pcdFile ReadMode
                               (pcdh,_) <- readHeader h
                               r <- pcdh `deepseq` readPointData pcdh h parser
                               hClose h
                               return $ either (const V.empty) id r

-- |Read a PCD file consisting of floating point XYZ coordinates for
-- each point.
loadXyz :: (Fractional a, Storable a) => FilePath -> IO (Vector (V3 a))
loadXyz = loadPoints A.readXYZ
{-# SPECIALIZE loadXyz :: FilePath -> IO (Vector (V3 Float)) #-}
{-# SPECIALIZE loadXyz :: FilePath -> IO (Vector (V3 Double)) #-}

-- |Read a PCD file consisting of floating point XYZW coordinates for
-- each point (where the final \"W\" field may be an RGB triple
-- encoded as a float).
loadXyzw :: (Fractional a, Storable a) => FilePath -> IO (Vector (V4 a))
loadXyzw = loadPoints A.readXYZW
{-# SPECIALIZE loadXyzw :: FilePath -> IO (Vector (V4 Float)) #-}
{-# SPECIALIZE loadXyzw :: FilePath -> IO (Vector (V4 Double)) #-}

-- |Load a 'B.Vector' of points, each represented as a 'B.Vector' of
-- 'FieldType' fields. If you wish to use field names to access to the
-- data, consider using 'loadFieldsByName'.
loadFlexiblePoints :: Header -> Handle -> IO (B.Vector (B.Vector FieldType))
loadFlexiblePoints pcdh h
  | pcdh ^. format == Binary = parseBinaryPoints pcdh h
  | otherwise = A.readPointsDefault pcdh h

-- |Parse every field of every point in a PCD file. Returns a function
-- that may be used to project out a named field.
loadFieldsByName :: FilePath -> IO (Text -> B.Vector FieldType)
loadFieldsByName f = do h <- openFile f ReadMode
                        (pcdh,_) <- readHeader h
                        (mkProjector pcdh <$> loadFlexiblePoints pcdh h) 
                          <* hClose h
  where mkProjector :: Header -> B.Vector (B.Vector FieldType) -> 
                       (Text -> B.Vector FieldType)
        mkProjector h pts = let fieldNames = B.fromList $ h ^. fields
                            in \name -> maybe B.empty 
                                              (flip B.map pts . flip (B.!))
                                              (B.findIndex (name==) fieldNames)
