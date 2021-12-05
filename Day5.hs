module Day5 where
import Data.List
import Data.Tuple
import Data.List.Split
import qualified Data.Set as S
 
type Point = (Int, Int)
type Segment = (Point, Point)
type Vector = (Int, Int)

start, end :: Segment -> Point
start = fst
end   = snd

minus :: Point -> Point -> Vector
minus pend pbeg = (fst pend - fst pbeg, snd pend - snd pbeg)

parse :: [[String]] -> [Segment]
parse = fmap parseLn
  where
    parseLn :: [String] -> Segment
    parseLn (sp1: _: sp2 : []) = 
      let 
          (x1 : y1 : []) = splitOn "," sp1
          (x2 : y2 : []) = splitOn "," sp2
      in ((readInt x1, readInt y1), (readInt x2, readInt y2))
      
-- Assume all segments
-- are either horizontal, vertical, or diagonal

-- Line is: starting point, direction, steps
line :: Segment -> (Point, Vector, Int)
line seg = (pt, v, n)
  where
    pt = start seg
    (v, n) = normalize (end seg `minus` start seg)
    normalize :: Vector -> (Vector, Int)
    normalize (vx, vy) = ((signum vx, signum vy), len)
      where 
        len = if vx == 0 then abs vy else abs vx
    
isDiag :: (Point, Vector, Int) -> Bool
isDiag (_, (vx, vy), _) = vx /= 0 && vy /= 0

drawLine :: (Point, Vector, Int) -> [Point]
drawLine ((x, y), (vx, vy), len) = 
  [ (x + n * vx, y + n * vy) | n <- [0 .. len]]
  
type PointSet = S.Set Point

overlap :: [Point] -> PointSet
overlap = snd . foldr ins (S.empty, S.empty) 
  where
    ins :: Point -> (PointSet, PointSet) -> (PointSet, PointSet)
    ins p (set, lst) = 
      if S.member p set
      then (set, S.insert p lst)
      else (S.insert p set, lst)

solve1 :: [Segment] -> Int
solve1 = length . overlap . concat . fmap drawLine . filter (not . isDiag) . fmap line

solve2 :: [Segment] -> Int
solve2 = length . overlap . concat . fmap drawLine . fmap line
 
readInt :: String -> Int
readInt s = read s
  
main :: IO ()
main = do
  text <- readFile "data5.txt"
  let ws = fmap words $ lines text
  let segs = parse ws
  print $ solve1 segs
  print $ solve2 segs
