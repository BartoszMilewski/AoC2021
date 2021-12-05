{-# LANGUAGE ScopedTypeVariables #-}
module Day4 where
import Data.List
import Data.List.Split
import Data.Maybe

type Board = [[(Int, Bool)]]

--splitWhen :: (a -> Bool) -> [a] -> [[a]] 
 
parse :: [String] -> ([Int], [Board])
parse (l1: _ : ls) = (nums, boards)
  where
    nums = fmap readInt $ splitOn "," l1
    bs :: [[String]]
    bs = splitWhen null ls
    boards = fmap parseBoard bs
    
parseBoard :: [String] -> Board
parseBoard lns = fmap (fmap (\n -> (n, False))) rows
  where rows = fmap (fmap readInt) $ fmap words lns
  
readInt :: String -> Int
readInt s = read s

mark :: Int -> [Board] -> [Board]
mark n = fmap (fmap (fmap (go n)))
  where 
    go :: Int -> (Int, Bool) -> (Int, Bool)
    go n (m, b) = if n == m then (m, True) else (m, b)
    
winHor :: Board -> Bool
winHor = any winRow
  where
    winRow = all (\(_, b) -> b)
    
winVer :: Board -> Bool
winVer = winHor . transpose

win :: Board -> Bool
win board = winHor board || winVer board

-- Any of the boards winning?
hasWin :: [Board] -> Bool
hasWin bs = or (fmap win bs)


-- Create complete history of board positions and numbers called
runNumbers :: [Board] -> [Int] -> [([Board], Int)]
runNumbers bs ns = scanl go (bs, -1) ns
  where
    go :: ([Board], Int) -> Int -> ([Board], Int)
    go (b, _) n = (mark n b, n)

-- Run through history, find the first winning board and number
findWin :: [([Board], Int)] -> (Board, Int)
findWin bss = 
  case find hasWinNum bss of
    Nothing -> error "No winner"
    Just (bs, n) -> (fromJust (find win bs), n)
  where 
    hasWinNum :: ([Board], Int) -> Bool
    hasWinNum (bs, _) = hasWin bs
  
-- last board to win 
lastWin :: [Board] -> [Int] -> (Board, Int)
lastWin bss ns = (head lastWins, lastN)
  where
    bss' :: [([Board], [Board], Int)]
    bss' = eliminate bss ns
    (_, lastWins, lastN) = fromJust $ find (\(loses, wins, n) -> null loses) bss'

-- Create complete history but keep only losing boards and the last winning boards
eliminate :: [Board] -> [Int] -> [([Board], [Board], Int)]
eliminate bs ns = scanl go (bs, [], -1) ns
  where
    go :: ([Board], [Board], Int) -> Int -> ([Board], [Board], Int)
    go (bs, _, _) n = (loseBs, winBs, n)
      where
        (winBs, loseBs) = partition win (mark n bs)
    
  
score :: (Board, Int) -> Int
score (b, num) = num * sum (fmap sumRow b)
  where
    sumRow :: [(Int, Bool)] -> Int
    sumRow = sum . fmap fst . filter (not . snd)

solve1 :: [Board] -> [Int] -> Int
solve1 boards nums =  
  let marked = runNumbers boards nums 
      -- skip the first 4 iterations
      winner = findWin (drop 4 marked)
  in score winner

main :: IO ()
main = do
  text <- readFile "data4.txt"
  let (nums, boards) = parse (lines text)
  print $ solve1 boards nums
  print $ score $ lastWin boards nums
  
