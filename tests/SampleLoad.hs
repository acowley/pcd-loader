-- |Ensure that a sample PCD file can be loaded.
module Main where
import Control.Lens ((^.))
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Directory (getTemporaryDirectory)
import System.IO (openTempFile, hClose, hSeek, SeekMode(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), (~?), (~=?))

import SampleData
import PCD.Data
import PCD.Header

testLen :: Header -> V.Vector (V.Vector FieldType) -> Test
testLen header pts = TestLabel "Number of fields" $ 
                     TestList [ 4 ~=? length (header^.fields)
                              , 4 ~=? V.length (V.head pts) ]

testPts :: Header -> V.Vector a -> Test
testPts header pts = TestLabel "Number of points" $
                     TestList [ 213 ~=? header^.points 
                              , 213 ~=? V.length pts ]

within :: (Num a, Ord a) => a -> a -> a -> Bool
within delta x y = abs (y - x) < delta

testData :: V.Vector (V.Vector FieldType) -> Test
testData pts = TestLabel "Point data" $
               TestList [ within 0.001 (-0.23729) lastY ~? 
                            "Last point's Y coordinate" ]
  where lastY = unsafeUnwrap (V.last pts V.! 1) :: Float

main :: IO ()
main = do d <- getTemporaryDirectory
          (_, h) <- openTempFile d "pcdTest"
          T.hPutStr h sampleData
          hSeek h AbsoluteSeek 0
          (header, _) <- readHeader h
          pts <- loadFlexiblePoints header h
          defaultMain . hUnitTestToTests $
            TestList [testLen header pts, testPts header pts, testData pts]
          hClose h
