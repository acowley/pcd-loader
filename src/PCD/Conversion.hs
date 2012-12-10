-- |Facility to convert an ASCII PCD file to a Binary one.
module PCD.Conversion where
import Control.Lens ((^.), (.~))
import Control.Monad (when)
import qualified Data.Text.IO as T
import qualified Data.Vector as B
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO (IOMode(..), openFile, hClose, hPutBuf, withBinaryFile)

import PCD.Header
import qualified PCD.Internal.AsciiParsers as A
import PCD.Internal.StorableFieldType (pokeBinaryPoints)

-- |@asciiToBinary inputFile outputFile@ converts a PCD file from
-- ASCII to Binary.
asciiToBinary :: FilePath -> FilePath -> IO ()
asciiToBinary i o = do h <- openFile i ReadMode
                       (pcdh,_) <- readHeader h
                       print pcdh
                       when (pcdh^.format /= ASCII)
                            (error "Input PCD is already binary!")
                       let numBytes = totalBinarySize pcdh
                       putStrLn $ "Expecting to generate "++show numBytes++" bytes"
                       v <- A.readPointsDefault pcdh h
                       putStrLn $ "Parsed "++show (B.length v)++" ASCII points"
                       hClose h
                       T.writeFile o (writeHeader (format .~ Binary $ pcdh))
                       withBinaryFile o AppendMode $ \h' ->
                         allocaBytes numBytes $ \ptr ->
                           pokeBinaryPoints ptr v >>
                           hPutBuf h' ptr numBytes
