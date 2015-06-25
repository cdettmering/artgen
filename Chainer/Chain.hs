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
import qualified Chainer.ProbabilityMap as ProbabilityMap
import qualified System.Random as Random

{-
 - Gives back a Marchov Chained list generated from the source list
 - This function will take care of random number generator construction.
-}
fromListIO :: (Ord a) => [a] -> IO [a]
fromListIO lst = do
                     g <- Random.newStdGen
                     let (s, gen) = start lst g
                     let c = fromList lst s gen
                     return $ c

{-
 - Same as fromListIO except the seed and random number generator must
 - be supplied
-}
fromList :: (Ord a, Random.RandomGen g) => [a] -> a -> g -> [a]
fromList lst s g = let p = (ProbabilityMap.fromList lst) in take (length lst) (chain s p g)

{-
 - Gives back a Marchov Chained list generated form the source list list.
 - This function will merge the ProbabilityMap's of each input list and 
 - then generate the output
-}
fromListListIO :: (Ord a) => [[a]] -> IO [a]
fromListListIO lst = do
                         g <- Random.newStdGen
                         let (s, gen) = startListList lst g
                         let c = fromListList lst s gen
                         return $ c

{-
 - Takes in multiple inputs and merges them together into 1 chained output
-}
fromListList :: (Ord a, Random.RandomGen g) => [[a]] -> a -> g -> [a]
fromListList lst s g = let p = (foldr (\x acc -> ProbabilityMap.merge (ProbabilityMap.fromList x) acc) ProbabilityMap.empty lst) in take (averageLength lst) (chain s p g)

sumLength :: [[a]] -> Int
sumLength lst = foldr (\x acc -> (length x) + acc) 0 lst

averageLength :: [[a]] -> Int
averageLength lst = quot (sumLength lst) (length lst)

{-
 - Generates an infinite Marchov Chain based off of the supplied probability map
 - and seed.
-}
chain :: (Ord a, Random.RandomGen g) => a -> ProbabilityMap.ProbabilityMap a -> g -> [a]
chain seed p gen =  let (a, g) = (next seed p gen) in seed : (chain a p g)

{-
 - picks the next number in the chain based off of probability
-}
next :: (Ord a, Random.RandomGen g) => a -> ProbabilityMap.ProbabilityMap a -> g -> (a, g)
next a p g = ProbabilityMap.pick a p g

{-
 - Picks random starting seed given a list list
-}
startListList :: (Ord a, Random.RandomGen g) => [[a]] -> g -> (a, g)
startListList lst gen = let s = (foldr (++) [] lst) in start s gen

{-
 - Picks a random starting seed
-}
start :: (Ord a, Random.RandomGen g) => [a] -> g -> (a, g)
start lst gen = let (a, g) = Random.randomR (0, (length lst) - 1) gen in (lst !! a, g)
