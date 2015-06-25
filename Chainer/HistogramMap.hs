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

module Chainer.HistogramMap where
import qualified Chainer.Histogram as Histogram
import qualified Data.Map as Map

-- Maps an element to its successor histogram, where a successor histogram
-- is the histogram of the element that follows the current element.
type HistogramMap a = Map.Map a (Histogram.Histogram a)

{-
 - Creates an empty HistogramMap
-}
empty :: HistogramMap a
empty = Map.empty :: Map.Map a (Histogram.Histogram a)

{-
 - Creates a HistogramMap from a list
-}
fromList :: Ord a => [a] -> HistogramMap a
fromList [] = empty
fromList (x:[]) = empty
fromList (x:y:z) = adjustOrInsert (\h -> Histogram.add y h) x (fromList (y:z))

{-
 - Merges 2 HistogramMaps together
-}
merge :: Ord a => HistogramMap a -> HistogramMap a -> HistogramMap a
merge histogram1 histogram2 = Map.unionWith (\element1 element2 -> Histogram.merge element1 element2) histogram1 histogram2

{-
 - Inserts element into the HistogramMap if it doesn't exist, otherwise adjusts
 - the value of element by applying f to it.
-}
adjustOrInsert :: Ord k => (Histogram.Histogram k -> Histogram.Histogram k) -> k -> HistogramMap k -> HistogramMap k
adjustOrInsert f element histogram = case (Map.lookup element histogram) of
                                         Just found -> Map.adjust f element histogram
                                         Nothing -> Map.insert element (f Histogram.empty) histogram

{-
 - Gets the Histogram for the key, otherwise returns an empty
 - Histogram if key doesn't exist.
-}
histogram :: Ord a => a -> HistogramMap a -> Histogram.Histogram a
histogram key histogram = case (Map.lookup key histogram) of
                              Just found -> found
                              Nothing -> Histogram.empty
