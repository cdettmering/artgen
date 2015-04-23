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
import qualified Chainer.Histogram as H
import qualified Data.Map as M

-- Maps an element to its successor histogram, where a successor histogram
-- is the histogram of the element that follows the current element.
type HistogramMap a = M.Map a (H.Histogram a)

{-
 - Creates an empty HistogramMap
-}
empty :: HistogramMap a
empty = M.empty :: M.Map a (H.Histogram a)

{-
 - Creates a HistogramMap from a list
-}
fromList :: Ord a => [a] -> HistogramMap a
fromList [] = empty
fromList (x:[]) = empty
fromList (x:y:z) = adjustOrInsert (\h -> H.add y h) x (fromList (y:z))

{-
 - Inserts k into the HistogramMap if it doesn't exist, otherwise adjusts
 - the value of k by applying f to it.
-}
adjustOrInsert :: Ord k => (H.Histogram k -> H.Histogram k) -> k -> HistogramMap k -> HistogramMap k
adjustOrInsert f k h = case (M.lookup k h) of
                           Just found -> M.adjust f k h
                           Nothing -> M.insert k (f H.empty) h

{-
 - Gets the Histogram for the key a, otherwise returns an empty
 - Histogram is k doesn't exist.
-}
histogram :: Ord a => a -> HistogramMap a -> H.Histogram a
histogram a h = case (M.lookup a h) of
                    Just found -> found
                    Nothing -> H.empty
