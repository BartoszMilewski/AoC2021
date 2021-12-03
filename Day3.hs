{-# LANGUAGE DeriveFunctor #-}
module Day3 where
  
import Data.List

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

newtype Fix f = Fix { unFix :: (f (Fix f)) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coa = Fix . fmap (ana coa) . coa

hylo :: Functor f => Coalgebra f a -> Algebra f b -> a -> b
hylo coa alg = alg . fmap (hylo coa alg) . coa 

---------
data TreeF x
  = Leaf [Int]
  | Node (Int, x) (Int, x)
  deriving Functor

-- Radix tree storing the number of left and right leaves
coa :: Coalgebra TreeF [[Int]]
coa [] = Leaf []
coa dss = 
  if length dss == 1
  then Leaf (head dss)
  else Node (length left, left) (length right, right)
  -- leading 0s on the left, leading 1s on the right
  where (left, right) = foldr f ([], []) dss
        f [] accs = accs
        f (d : ds) (lacc, racc) = 
          if d == 0 then (ds : lacc, racc)
                    else (lacc,      ds : racc)

parse :: [String] -> [[Int]]
parse = fmap toDigs
  where
    toDigs :: String -> [Int]
    toDigs = fmap toDig
    toDig c = if c == '0' then 0 else 1
  
trans :: [[Int]] -> [Int]
trans ds = 
  let cols = transpose ds
      len  = length (head cols)
  in fmap (getMax len) cols
 where
  getMax :: Int -> [Int] -> Int
  getMax len bs = 
    if (sum bs) > len `div` 2 then 1 else 0
    
neg :: [Int] -> [Int]
neg = fmap (\x -> if x == 0 then 1 else 0)

toDec :: [Int] -> Int
toDec = foldl (\n d -> 2 * n + d) 0 

solve1 :: [[Int]] -> Int
solve1 dss = 
  let ds = trans dss
      x = toDec ds
      y = toDec (neg ds)
  in x * y
  
doMax :: (Fix TreeF) -> [Int]
doMax (Fix (Leaf ds)) = ds
doMax (Fix (Node (lc, lt) (rc, rt))) =
  if lc > rc 
  then 0 : doMax lt
  else 1 : doMax rt

doMin :: (Fix TreeF) -> [Int]
doMin (Fix (Leaf ds)) = ds
doMin (Fix (Node (lc, lt) (rc, rt))) =
  if lc <= rc 
  then 0 : doMin lt
  else 1 : doMin rt
  
solve2 :: [[Int]] -> Int
solve2 dss = toDec (doMin tree) * toDec (doMax tree)
  where tree = ana coa dss

main :: IO ()
main = do
  text <- readFile "data3.txt"
  let dss = parse (lines text)
  print $ solve1 dss
  print $ solve2 dss
  
-- For testing

showAlg :: Algebra TreeF String
showAlg (Leaf ds) = "<" ++ show ds ++ ">"
showAlg (Node (n1, s1) (n2, s2)) =
  "(" ++ show n1 ++ ", " ++ s1 ++ ")\n(" ++ show n2 ++ ", " ++ s2 ++ ")"
       
showTree = cata showAlg                   
