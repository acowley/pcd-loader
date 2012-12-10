{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import System.Environment (getArgs)
import PCD.Conversion (asciiToBinary)
import PCD.Data (projectBinaryFields)

data Args = Args { _inputFile  :: FilePath
                 , _outputFile :: FilePath
                 , _justXyz    :: Bool }

parseArgs :: [String] -> Maybe Args
parseArgs ["-xyz", a, b] = Just $ Args a b True
parseArgs [a,b] = Just $ Args a b False
parseArgs _ = Nothing

usage :: IO ()
usage = do putStrLn "Usage: pcd2bin [-xyz] asciiPcd outputBinaryFile"
           putStrLn "- The '-xyz' option restricts output to those fields."

main :: IO ()
main = getArgs >>= maybe usage aux . parseArgs
  where aux (Args a b False) = asciiToBinary a b
        aux (Args a b True) = projectBinaryFields ["x","y","z"] a b
