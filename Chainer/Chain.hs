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

module Chainer.Chain where
import qualified Chainer.ProbabilityMap as P
import qualified Data.Map as M
import qualified Data.List as L
import qualified System.Random as R

{-
 - Gives back a Marchov Chained list generated from the source list
 - This function will take care of random number generator construction.
-}
fromListIO :: (Ord a) => [a] -> IO [a]
fromListIO lst = do
                     g <- R.newStdGen
                     let (s, gen) = start lst g
                     let c = fromList lst s gen
                     return $ c

{-
 - Same as fromListIO except the seed and random number generator must
 - be supplied
-}
fromList :: (Ord a, R.RandomGen g) => [a] -> a -> g -> [a]
fromList lst s g = let p = (P.fromList lst) in take (length lst) (chain s p g)

{-
 - Generates an infinite Marchov Chain based off of the supplied probability map
 - and seed.
-}
chain :: (Ord a, R.RandomGen g) => a -> P.ProbabilityMap a -> g -> [a]
chain seed p gen =  let (a, g) = (next seed p gen) in seed : (chain a p g)

{-
 - picks the next number in the chain based off of probability
-}
next :: (Ord a, R.RandomGen g) => a -> P.ProbabilityMap a -> g -> (a, g)
next a p g = P.pick a p g

{-
 - Picks a random starting seed
-}
start :: (Ord a, R.RandomGen g) => [a] -> g -> (a, g)
start lst gen = let (a, g) = R.randomR (0, (length lst) - 1) gen in (lst !! a, g)
