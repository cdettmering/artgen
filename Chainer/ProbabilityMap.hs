{-
This file is part of artgen.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

Copyright (c) 2015 Chad Dettmering

Authors:
    Chad Dettmering     chad.dettmering@gmail.com
-}

module Chainer.ProbabilityMap where
import qualified Chainer.Probability as Probability
import qualified Chainer.HistogramMap as HistogramMap
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.Random as Random

type ProbabilityMap a = Map.Map a (Probability.Probability a)

{-
 - Creates an empty ProbabilityMap
-}
empty :: ProbabilityMap a
empty = Map.empty :: Map.Map a (Probability.Probability a)

{-
 - Creates a ProbabilityMap from a list
-}
fromList :: Ord a => [a] -> ProbabilityMap a
fromList lst = fromHistogramMap (HistogramMap.fromList lst)

{-
 - Creates a ProbabilityMap from a HistogramMap
-}
fromHistogramMap :: Ord a => HistogramMap.HistogramMap a -> ProbabilityMap a
fromHistogramMap h = foldr (\hKey hAcc -> (let histogram = HistogramMap.histogram hKey  h in 
                                              let probability = Probability.fromHistogram histogram in
                                                  adjustOrInsert (\p -> Probability.add hKey histogram probability) hKey hAcc)) empty (Map.keys h)

{-
 - Merges 2 ProbabilityMap's together
-}
merge :: Ord a => ProbabilityMap a -> ProbabilityMap a -> ProbabilityMap a
merge p1 p2 = Map.unionWith (\x1 x2 -> Probability.merge x1 x2) p1 p2
                             
{-
 - Inserts k into the ProbabilityMap if it doesn't exist, otherwise adjusts
 - the value of k by applying f to it.
-}
adjustOrInsert :: Ord k => (Probability.Probability k -> Probability.Probability k) -> k -> ProbabilityMap k -> ProbabilityMap k
adjustOrInsert f k p = case (Map.lookup k p) of
                           Just found -> Map.adjust f k p
                           Nothing -> Map.insert k (f Probability.empty) p

{-
 - Picks the successor of a based off of the ProbabilityMap. If
 - a is not found in the probability map then a random successor
 - will be chosen.
-}
pick :: (Ord a, Random.RandomGen g) => a -> ProbabilityMap a -> g -> (a, g)
pick a p  g = case (Map.lookup a p) of
                 Just x -> (Probability.pick x g)
                 -- Picks a random element from the probability map
                 Nothing -> (Probability.pick (Maybe.fromJust(Map.lookup (head (Map.keys p)) p)) g)
