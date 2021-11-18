module Almanac.Import where
import Data.Foldable (foldMap')

-- | Flipped 'map'. I've done too much Javascript.
forEach :: [a] -> (a -> b) -> [b]
forEach = flip map

-- | Flipped 'foldMap\''. 
concatForEach :: Monoid a1 => [a2] -> (a2 -> a1) -> a1
concatForEach = flip foldMap'
