{-# LANGUAGE BangPatterns #-}
module PCD.Internal.AsciiParsers where
import Control.Applicative
import Control.Lens ((^.))
import Data.Attoparsec.Text (double, count, skipSpace)
import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as B
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.IO (Handle)

import PCD.Header (Header, FieldType, pointParser, points)
import PCD.Internal.Types (V3(..), V4(..))

-- |Read point data using a user-supplied ASCII point parser.
readPoints :: (G.Vector v a) => 
                   Header -> Handle -> ATL.Parser a -> IO (v a)
readPoints pcd h p = aux <$> TL.hGetContents h
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

-- |Load points of arbitrary dimension into a boxed vector with a
-- 'B.Vector' of 'FieldType' as the point representation.
readPointsDefault :: Header -> Handle -> IO (B.Vector (B.Vector FieldType))
readPointsDefault pcd h = readPoints pcd h $ B.fromList <$> pointParser pcd

-- |Parse 3D points serialized in ASCII.
readXYZ :: Fractional a => ATL.Parser (V3 a)
readXYZ = (\[x,y,z] -> V3 x y z) <$> 
          count 3 ((realToFrac <$> double) <* skipSpace)

-- |Parse 4D points serialized to ASCII. This is useful for points
-- with X,Y,Z, and RGB fields each represented by a single float.
readXYZW :: Fractional a => ATL.Parser (V4 a)
readXYZW = (\[x,y,z,w] -> V4 x y z w) <$>
           count 4 ((realToFrac <$> double) <* skipSpace)
