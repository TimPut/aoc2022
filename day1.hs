import Data.List.Split
import Data.List
import Data.Ord
import Control.Arrow

main = do
  let f = "d1input.txt"
  print =<< ((!! 1) &&& (!! 3)) . scanl (+) 0 . sortOn Down . fmap sum . fmap (fmap read) . fmap lines . splitOn "\n\n" <$> readFile f :: IO ()
