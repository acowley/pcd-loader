{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |Parser for PCD (point cloud data) files. Also provides a facility
-- for converting from ASCII to binary formatted point data.
module PCD.Data (FieldType(..), unsafeUnwrap, loadAllFields, 
                 loadXyzw, loadXyz, asciiToBinary, saveBinaryPcd, 
                 projectBinaryFields) where
import Control.Applicative
import Control.DeepSeq
import Control.Monad (when)
import Data.Attoparsec.Text hiding (I)
import qualified Data.Attoparsec.Text.Lazy as ATL
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.IO as T
import qualified Data.Vector as B
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable, sizeOf)
import System.IO (Handle, openFile, hClose, 
                  IOMode(..), withBinaryFile, hPutBuf, hGetBuf)
import PCD.Header
import PCD.Internal.SmallLens
import PCD.Internal.StorableFieldType
import PCD.Internal.Types

-- |Read point data using a user-supplied ASCII point parser.
readAsciiPoints :: (G.Vector v a) => 
                   Header -> Handle -> ATL.Parser a -> IO (v a)
readAsciiPoints pcd h p = aux <$> TL.hGetContents h
  where n = fromIntegral $ pcd^.points
        aux t0 = G.create $
                 do v <- GM.new n
                    let write = GM.write v
                        go !i !t
                          | i == n = return v
                          | otherwise = case ATL.parse p t of
                                          ATL.Done !t' !pt -> write i pt >> 
                                                              go (i+1) t'
                                          ATL.Fail _ _ msg -> error msg
                    go 0 t0

-- |Load points of unknown dimension into a boxed vector with a list
-- of 'FieldType' as the point representation.
readAsciiPointsDefault :: Header -> Handle -> IO (B.Vector (B.Vector FieldType))
readAsciiPointsDefault pcd h = readAsciiPoints pcd h $ 
                               B.fromList <$> pointParser pcd

-- |Read back 'Storable' points saved as binary data.
readHomogenousBinaryPoints :: forall a. Storable a => 
                              Header -> Handle -> IO (Either String (Vector a))
readHomogenousBinaryPoints pcd h
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
readPointData hd h pa 
  | hd^.format == ASCII = readAsciiPoints hd h pa >>= return . Right
  | otherwise = readHomogenousBinaryPoints hd h

-- |Parse 3D points serialized in ASCII.
readXYZ_ascii :: Fractional a => ATL.Parser (V3 a)
readXYZ_ascii = (\[x,y,z] -> V3 x y z) <$> 
                count 3 ((realToFrac <$> double) <* skipSpace)

-- |Parse 4D points serialized to ASCII. This is useful for points
-- with X,Y,Z, and RGB fields each represented by a single float.
readXYZW_ascii :: Fractional a => ATL.Parser (V4 a)
readXYZW_ascii = (\[x,y,z,w] -> V4 x y z w) <$>
                 count 4 ((realToFrac <$> double) <* skipSpace)

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

-- |@asciiToBinary inputFile outputFile@ converts a PCD file from
-- ASCII to Binary.
asciiToBinary :: FilePath -> FilePath -> IO ()
asciiToBinary i o = do h <- openFile i ReadMode
                       (pcdh,_) <- readHeader h
                       pcdh `deepseq` print pcdh
                       when (pcdh^.format /= ASCII)
                            (error "Input PCD is already binary!")
                       let numBytes = totalBinarySize pcdh
                       putStrLn $ "Expecting to generate "++show numBytes++" bytes"
                       v <- readAsciiPointsDefault pcdh h
                       putStrLn $ "Parsed "++show (B.length v)++" ASCII points"
                       hClose h
                       T.writeFile o (writeHeader (format .~ Binary $ pcdh))
                       withBinaryFile o AppendMode $ \h' ->
                         allocaBytes numBytes $ \ptr ->
                           pokeBinaryPoints ptr v >>
                           hPutBuf h' ptr numBytes

-- |Save a binary PCD file including only the named fields.
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
-- 'Linear.V2.V2' or 'Linear.V3.V3' to represent points.
loadPoints :: Storable a => ATL.Parser a -> FilePath -> IO (Vector a)
loadPoints parser pcdFile = do h <- openFile pcdFile ReadMode
                               (pcdh,_) <- readHeader h
                               r <- pcdh `deepseq` readPointData pcdh h parser
                               hClose h
                               return $ either (const V.empty) id r

-- |Read a PCD file consisting of floating point XYZ coordinates for
-- each point.
loadXyz :: (Fractional a, Storable a) => FilePath -> IO (Vector (V3 a))
loadXyz = loadPoints readXYZ_ascii
{-# SPECIALIZE loadXyz :: FilePath -> IO (Vector (V3 Float)) #-}
{-# SPECIALIZE loadXyz :: FilePath -> IO (Vector (V3 Double)) #-}

-- |Read a PCD file consisting of floating point XYZW coordinates for
-- each point (where the final \"W\" field may be an RGB triple
-- encoded as a float).
loadXyzw :: (Fractional a, Storable a) => FilePath -> IO (Vector (V4 a))
loadXyzw = loadPoints readXYZW_ascii
{-# SPECIALIZE loadXyzw :: FilePath -> IO (Vector (V4 Float)) #-}
{-# SPECIALIZE loadXyzw :: FilePath -> IO (Vector (V4 Double)) #-}

loadFlexiblePoints :: Header -> Handle -> IO (B.Vector (B.Vector FieldType))
loadFlexiblePoints pcdh h
  | pcdh ^. format == Binary = parseBinaryPoints pcdh h
  | otherwise = readAsciiPointsDefault pcdh h

-- |Parse every field of every point in a PCD file. Returns a function
-- that may be used to project out a named field.
loadAllFields :: FilePath -> IO (Text -> B.Vector FieldType)
loadAllFields f = do h <- openFile f ReadMode
                     (pcdh,_) <- readHeader h
                     (mkProjector pcdh <$>
                       if pcdh ^. format == ASCII
                         then readAsciiPoints pcdh h 
                                              (B.fromList <$> pointParser pcdh)
                         else parseBinaryPoints pcdh h)
                       <* hClose h
  where mkProjector :: Header -> B.Vector (B.Vector FieldType) -> 
                       (Text -> B.Vector FieldType)
        mkProjector h pts = let fieldNames = B.fromList $ h ^. fields
                            in \name -> maybe B.empty 
                                              (flip B.map pts . flip (B.!))
                                              (B.findIndex (name==) fieldNames)
