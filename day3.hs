import Data.List.Split
import Data.List
import Data.Ord
import Control.Arrow
import qualified Data.Set as S
import Data.Char

main = do
  let f = "d3input.txt"
  print =<< sum . fmap (priority . S.toList . uncurry S.intersection) . fmap (S.fromList *** S.fromList) . fmap bisect . lines <$> readFile f :: IO ()
  print =<< sum . fmap (priority . S.toList . foldr1 S.intersection) . chunksOf 3 . fmap S.fromList . lines <$> readFile f :: IO ()

bisect :: [a] -> ([a],[a])
bisect l = splitAt (length l `div` 2) l

priority [c] = if c <= 'Z'
             then ord c - ord 'A' + 27
             else ord c - ord 'a' + 1
