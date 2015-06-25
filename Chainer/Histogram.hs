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

module Chainer.Histogram where
import qualified Data.Map as Map

-- Maps an element to the number of occurrences of that element.
type Histogram a = Map.Map a Integer

{-
 - Creates an empty Histogram
-}
empty :: Histogram a
empty = Map.empty :: Map.Map a Integer

{-
 - Creates a Histogram from a list
-}
fromList :: Ord a => [a] -> Histogram a
fromList lst = foldr (\x acc -> add x acc) empty lst

{-
 - Merges 2 Histograms together adding their occurrences
-}
merge :: Ord a => Histogram a -> Histogram a -> Histogram a
merge h1 h2 = Map.unionWith (+) h1 h2

{-
 - Adds a value to the Histogram, or adjust the value
 - +1 if it already exists in the Histogram
-}
add :: Ord a => a -> Histogram a -> Histogram a
add value histogram = adjustOrInsert (\x -> x + 1) value histogram

{-
 - Gets the number of occurrences of the element within
 - the Histogram
-}
occurrences :: Ord k => k -> Histogram k -> Integer
occurrences element histogram = case (Map.lookup element histogram) of
                                    Just found -> found
                                    Nothing -> 0

{-
 - Total sum of all Histogram occurrences
-}
sumOccurrences :: Histogram a -> Integer
sumOccurrences histogram = Map.fold (+) 0 histogram

{-
 - Inserts element into the Histogram if it doesn't exist, otherwise adjusts
 - the value of element by applying f to it.
-}
adjustOrInsert :: Ord k => (Integer -> Integer) -> k -> Histogram k -> Histogram k
adjustOrInsert f element histogram = case (Map.lookup element histogram) of
                                         Just found -> Map.adjust f element histogram
                                         Nothing -> Map.insert element (f 0) histogram
