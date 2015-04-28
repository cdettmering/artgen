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

{-
 - This is an example of what the Chainer module can do. This program
 - generates statistically similar text to the one provided. Try running
 - it on your favorite plain text books from project gutenberg!
-}
import qualified Data.Char as CH
import qualified Data.Text as T
import qualified Chainer.Chain as C
import qualified Chainer.ProbabilityMap as P
import qualified System.Environment as E
import qualified System.Directory as D

{-
 - Filters out non printable characters
-}
filterNonPrintable :: T.Text -> T.Text
filterNonPrintable s = T.filter (\c -> CH.isPrint c) s

{-
 - Adds in a newline every jth element
-}
addNewLines :: Int -> Int -> [T.Text] -> [T.Text]
addNewLines _ _ [] = []
addNewLines i j (t:ts) = if i == j then
                             (T.snoc t '\n') : (addNewLines 0 j ts)
                         else
                             t : (addNewLines (i + 1) j ts)

{-
 - Determines if a Char is a type of punctuation.
-}
isPunctuation :: Char -> Bool
isPunctuation c = c == '.' ||
                  c == ',' ||
                  c == ';' ||
                  c == ':' ||
                  c == '!' ||
                  c == '?' ||
                  c == '\"' ||
                  c == '\''

isSpecial :: Char -> Bool
isSpecial c = c == '*' ||
              c == '#' ||
              c == '@'

isUpper :: T.Text -> Bool
isUpper t = T.all CH.isUpper t

filterAllCaps :: [T.Text] -> [T.Text]
filterAllCaps t = filter (not . isUpper) t

filterSpecial :: T.Text -> T.Text
filterSpecial t = T.filter (not . isSpecial) t

{-
 - Separates punctuation into their own words
-}
separatePunctuation :: [T.Text] -> [T.Text]
separatePunctuation [] = []
separatePunctuation (t:ts) = if (isPunctuation (T.last t)) then
                                 (T.init t) : ((T.singleton . T.last) t) : (separatePunctuation ts)
                             else
                                t : (separatePunctuation ts)

{-
 - Merges any Char satisfying the predicate to the left
-}
merge:: (Char -> Bool) -> [T.Text] -> [T.Text]
merge _ [] = []
merge f (t:p:ts) = if (T.length p) == 1 then
                       -- This is a char
                       if (f (head (T.unpack p))) then
                           -- Predicate found merge it
                           (T.append t p) : (merge f ts)
                       else
                           t : p : (merge f ts)
                   else
                       t : p : (merge f ts)

mergePunctuation :: [T.Text] -> [T.Text]
mergePunctuation t = merge isPunctuation t


main :: IO ()
main = do
            -- Program takes 2 command line arguments:
            -- 1) The input directory that contains the input files (input files must be plain text)
            -- 2) The output file name
           (input:output:args) <- E.getArgs

           -- Get all directory contents filter . and ..
           !d <- fmap (filter (\x -> not (x == "." || x == ".."))) (D.getDirectoryContents input)

           -- Append input directory with contents
           let !files = map (input ++) d

           -- Read each file
           !tList <- mapM readFile files

           -- Pack into text, strip out leading and trailing whitespace, and then filter out non printable characters
           let !t = map (filterSpecial . filterNonPrintable . T.strip . T.pack) tList

           -- Split into words
           let !words = foldr (\x acc -> ((filterAllCaps . separatePunctuation . T.words) x) : acc) [[]] t

           -- Run the chain algorithm
           !chain <- C.fromListListIO words

           -- Merge back into a single string
           let !finalText = (T.unpack . T.unwords . (addNewLines 0 15)) chain

           -- Output to file
           writeFile output finalText
