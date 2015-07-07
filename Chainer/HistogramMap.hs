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
import qualified Data.Map.Strict as Map

-- Maps an element to its successor histogram, where a successor histogram
-- is the histogram of the element that follows the current element.
type HistogramMap a = Map.Map a (Histogram.Histogram a)

{-
 - Creates an empty HistogramMap
-}
emptyHM :: HistogramMap a
emptyHM = Map.empty :: Map.Map a (Histogram.Histogram a)

{-
 - Creates a HistogramMap from a list
-}
fromListHM :: Ord a => [a] -> HistogramMap a
fromListHM [] = emptyHM
fromListHM (x:[]) = emptyHM
fromListHM (x:y:z) = adjustOrInsertHM (\h -> Histogram.addH y h) x (fromListHM (y:z))

{-
 - Merges 2 HistogramMaps together
-}
mergeHM :: Ord a => HistogramMap a -> HistogramMap a -> HistogramMap a
mergeHM histogram1 histogram2 = Map.unionWith (\element1 element2 -> Histogram.mergeH element1 element2) histogram1 histogram2

{-
 - Inserts element into the HistogramMap if it doesn't exist, otherwise adjusts
 - the value of element by applying f to it.
-}
adjustOrInsertHM :: Ord k => (Histogram.Histogram k -> Histogram.Histogram k) -> k -> HistogramMap k -> HistogramMap k
adjustOrInsertHM f element histogram = case (Map.lookup element histogram) of
                                         Just found -> Map.adjust f element histogram
                                         Nothing -> Map.insert element (f Histogram.emptyH) histogram

{-
 - Gets the Histogram for the key, otherwise returns an empty
 - Histogram if key doesn't exist.
-}
histogram :: Ord a => a -> HistogramMap a -> Histogram.Histogram a
histogram key histogram = case (Map.lookup key histogram) of
                              Just found -> found
                              Nothing -> Histogram.emptyH
