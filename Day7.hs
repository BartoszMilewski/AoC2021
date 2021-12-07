module Day7 where
import Data.List
import Data.List.Split

readInt :: String -> Int
readInt s = read s

try :: [Int] -> Int -> Int
try ns n = sum $ fmap (\x -> abs (x - n)) ns

tries ns = fmap (try ns) [0 .. mx]
  where mx = maximum ns
  
try2 :: [Int] -> Int -> Int
try2 ns n = sum $ fmap (cost n) ns
  where
    cost n x = 
      let dist = abs (n - x)
      in dist * (dist + 1) `div` 2
 
tries2 ns = fmap (try2 ns) [0 .. mx]
  where mx = maximum ns
  
-- do a binary search of a minimum
-- knowing that the function is convex 
findMin :: [Int] -> Int
findMin ns = go left val right
  where
    mid = length ns `div` 2
    left = take mid ns
    right = drop (mid + 1) ns
    val = ns !! mid
    go :: [Int] -> Int -> [Int] -> Int
    go lft v rgt =
      case (null lft, null rgt) of 
        (True, True) -> v
        (True, False) -> min v (head rgt)
        (False, True) -> min (head lft) v
        _ -> if last lft < v
             then findMin lft
             else if head rgt < v
                  then findMin rgt
                  else v
  
solve1, solve2 :: [Int] -> Int
solve1 = findMin . tries
solve2 = findMin . tries2

main :: IO ()
main = do
  text <- readFile "data7.txt"
  let ns = fmap readInt $ splitOn "," text
  print $ solve1 ns
  print $ solve2 ns
