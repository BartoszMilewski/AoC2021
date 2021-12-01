module Day1 where
import Control.Comonad
import qualified Data.List.NonEmpty as L
import Data.Maybe
  
diffs :: [Int] -> [Int]
diffs xs = fmap (uncurry (-)) $ zip xs (tail xs)

countNeg :: [Int] -> Int
countNeg = sum . fmap (\x -> if x < 0 then 1 else 0) 

-- NonEmpty is a Comonad
-- Use extend to produce sums of 3 elements

sum3 :: L.NonEmpty Int -> Maybe Int
sum3 (a L.:| as) = case as of 
  [] -> Nothing
  (b : []) -> Nothing
  (b : c : _) -> Just (a + b + c)

sums :: L.NonEmpty Int -> [Int]
sums = fmap fromJust . L.filter isJust . extend sum3 
  
readInt :: String -> Int
readInt s = read s
  
main :: IO ()
main = do
  text <- readFile "data1.txt"
  let xs = fmap readInt $ lines text
  print $ countNeg $ diffs xs
  print $ countNeg $ diffs $ sums (L.fromList xs)
