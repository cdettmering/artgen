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
import qualified Chainer.Probability as P
import qualified Chainer.HistogramMap as H
import qualified Data.Map as M
import qualified Data.Maybe as J  
import qualified System.Random as R

type ProbabilityMap a = M.Map a (P.Probability a)

{-
 - Creates an empty ProbabilityMap
-}
empty :: ProbabilityMap a
empty = M.empty :: M.Map a (P.Probability a)

{-
 - Creates a ProbabilityMap from a list
-}
fromList :: Ord a => [a] -> ProbabilityMap a
fromList lst = fromHistogramMap (H.fromList lst)

{-
 - Creates a ProbabilityMap from a HistogramMap
-}
fromHistogramMap :: Ord a => H.HistogramMap a -> ProbabilityMap a
fromHistogramMap h = foldr (\hKey hAcc -> (let histogram = H.histogram hKey  h in 
                                              let probability = P.fromHistogram histogram in
                                                  adjustOrInsert (\p -> P.add hKey histogram probability) hKey hAcc)) empty (M.keys h)

{-
 - Merges 2 ProbabilityMap's together
-}
merge :: Ord a => ProbabilityMap a -> ProbabilityMap a -> ProbabilityMap a
merge p1 p2 = M.unionWith (\x1 x2 -> P.merge x1 x2) p1 p2
                             
{-
 - Inserts k into the ProbabilityMap if it doesn't exist, otherwise adjusts
 - the value of k by applying f to it.
-}
adjustOrInsert :: Ord k => (P.Probability k -> P.Probability k) -> k -> ProbabilityMap k -> ProbabilityMap k
adjustOrInsert f k p = case (M.lookup k p) of
                           Just found -> M.adjust f k p
                           Nothing -> M.insert k (f P.empty) p

{-
 - Picks the successor of a based off of the ProbabilityMap. If
 - a is not found in the probability map then a random successor
 - will be chosen.
-}
pick :: (Ord a, R.RandomGen g) => a -> ProbabilityMap a -> g -> (a, g)
pick a p  g = case (M.lookup a p) of
                 Just x -> (P.pick x g)
                 -- Picks a random element from the probability map
                 Nothing -> (P.pick (J.fromJust(M.lookup (head (M.keys p)) p)) g)
