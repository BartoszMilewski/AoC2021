module Day6 where
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M

type Tally = M.Map Int Int

tally :: [Int] -> Tally
tally ns = foldr (\n t -> M.insertWith (+) n 1 t) t0 ns
  where t0 = M.fromList $ zip [0..8] (replicate 9 0)

step :: Tally -> Tally
step t = 
  let (zs : lst) = M.elems t
      -- shift tally to left
      t' = M.fromList (zip [0..8] lst)
      -- append new eights
      t'' = M.insert 8 zs t'
      -- add new sixes
  in M.adjust (+ zs) 6 t''


readInt :: String -> Int
readInt s = read s
  
main :: IO ()
main = do
  text <- readFile "data6.txt"
  let ns = fmap readInt $ splitOn "," text
  let t = tally ns
  print $ sum $ M.elems $ (iterate step t) !! 80
  print $ sum $ M.elems $ (iterate step t) !! 256
