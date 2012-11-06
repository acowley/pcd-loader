module Main (main) where
import Control.Monad (when)
import System.Environment (getArgs)
import PCD.Data (asciiToBinary)

main :: IO ()
main = do args@(~[inputFile, outputFile]) <- getArgs
          when (length args /= 2)
               (error "Usage: pcd2bin asciiPcd outputBinaryFile")
          asciiToBinary inputFile outputFile 
