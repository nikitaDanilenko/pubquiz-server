module Sheet.Interval ( 
    Interval, Base, Size, 
    mkInterval, isize, itake, idrop, splitTo, toList 
) where

data Interval = Interval { lower :: Int, upper :: Int }

toList :: Interval -> [Int]
toList i = [lower i .. upper i]

instance Show Interval where
  show (Interval l u) = concat ["[", show l, ", ", show u, "]"]

class HasInt a where
  int :: a -> Int

newtype Base = Base Int
newtype Size = Size Int

instance HasInt Base where
  int (Base i) = i

instance HasInt Size where
  int (Size i) = i

mkInterval :: Base -> Size -> Interval
mkInterval base lnth = Interval lower (lower + max size 0) where
  size = int lnth - 1
  lower = int base

isize :: Interval -> Int
isize i = 1 + upper i - lower i

itake :: Int -> Interval -> Interval
itake n i | n <= isize i = mkInterval (Base l) (Size n)
          | otherwise    = i
  where l = lower i

idrop :: Int -> Interval -> Interval
idrop n i | n < sz    = mkInterval (Base (l + n)) (Size (sz - n))
          | otherwise = Interval u u
  where (l, u) = (lower i, upper i)
        sz     = isize i

splitTo :: Int -> Interval -> [Interval]
splitTo limit i | l <= limit = [i]
                | otherwise = itake limit i : splitTo limit (idrop limit i)
  where l = isize i