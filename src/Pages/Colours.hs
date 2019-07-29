module Pages.Colours where

import Data.Colour      ( blend, Colour )
import Data.Colour.SRGB ( sRGB, toSRGB, RGB ( .. ) )
import Data.List        ( intercalate )
import GHC.Real         ( Ratio ( .. ) )

type Col = Colour Double

mkHTMLColours :: Int -> [String]
mkHTMLColours = map toHTML . mkColours

toHTML :: Col -> String
toHTML c = concat [
    "rgb(",
    intercalate ", " $ map show [
        channelRed rgb,
        channelGreen rgb,
        channelBlue rgb
    ],
    ")"
  ] where rgb = fmap (\v -> floor (255 * v) :: Int) (toSRGB c)

rainbow :: [Col]
rainbow = map (\(r, g, b) -> sRGB (mkDouble r) (mkDouble g) (mkDouble b)) [
  (255, 99, 132)
  , (255, 159, 64)
  , (255, 205, 86)
  , (75, 192, 150)
  , (54, 162, 235)
  , (73, 91, 191)
  , (153, 102, 255)
  ] where 
        mkDouble :: Integer -> Double 
        mkDouble k = fromIntegral k / 255.0

lowerUpper :: Rational -> (Integer, Integer)
lowerUpper (a :% b) = (a `div` b, a `div` b + 1)

mkPositions :: Int -> [Rational]
mkPositions k = map (\i -> (i * num) :% denom) [0 .. toInteger k - 1] where
    num = toInteger (length rainbow - 1)
    denom = toInteger k - 1

type Indexed a = ((Integer, Integer), a)

indexedRainbow :: [Indexed (Col, Col)]
indexedRainbow = 
    zipWith3 (\i c c' -> ((i, 1 + i), (c, c'))) [0 :: Integer ..] rainbow (tail rainbow)

mkColours :: Int -> [Col]
mkColours n 
    | n <= length rainbow = take n rainbow
    | otherwise = merge withBounds indexedRainbow where
        positions = mkPositions n
        withBounds = map (\r -> (lowerUpper r, r)) positions

        merge :: [Indexed Rational] -> [Indexed (Col, Col)] -> [Col]
        merge [] _ = []
        merge rest [] = replicate (length rest) (last rainbow)
        merge irs@(((i, j), r) : irsTail) ics@(((i', j'), (c, c')) : icsTail) 
            | (i, j) == (i', j') = blend (1 - (fromRational r - fromIntegral i)) c c' : merge irsTail ics
            | otherwise = merge irs icsTail