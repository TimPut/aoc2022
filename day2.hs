import Data.List.Split
import Data.List
import Data.Ord
import Control.Arrow

main = do
  let f = "d2input.txt"
  print =<< foldl1' (\(a,b) (c,d) -> (a+c,b+d)) . fmap (scoreGame &&& scoreGame') . fmap words . lines <$> readFile f :: IO ()

scoreGame [[i],[j]] = case (i,j) of
  ('A','X') ->  3 + 1
  ('A','Y') ->  6 + 2
  ('A','Z') ->  0 + 3
  ('B','X') ->  0 + 1
  ('B','Y') ->  3 + 2
  ('B','Z') ->  6 + 3
  ('C','X') ->  6 + 1
  ('C','Y') ->  0 + 2 
  ('C','Z') ->  3 + 3
  _ -> error "bad game"

scoreGame' [[i],[j]] = case (i,j) of
  ('A','X') ->  0 + 3
  ('A','Y') ->  3 + 1
  ('A','Z') ->  6 + 2
  ('B','X') ->  0 + 1
  ('B','Y') ->  3 + 2
  ('B','Z') ->  6 + 3
  ('C','X') ->  0 + 2 
  ('C','Y') ->  3 + 3
  ('C','Z') ->  6 + 1
  _ -> error "bad game"
