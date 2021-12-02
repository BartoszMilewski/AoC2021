module Day2 where
 
data Dir = Up | Dn | Fwd
  deriving Show


parse :: [[String]] -> [(Dir, Int)]
parse ws = fmap parseLn ws
  where
    parseLn (d : x : _) = (parseDir d, readInt x)
    parseDir s = case s of
      "forward" -> Fwd
      "down" -> Dn
      "up" -> Up
      
exec :: [(Dir, Int)] -> (Int, Int)
exec = foldl mv (0, 0)
  where
    mv :: (Int, Int) -> (Dir, Int) -> (Int, Int)
    mv (x, y) (dir, v) =
      case dir of
        Dn  -> (x, y + v)
        Up  -> (x, y - v)
        Fwd -> (x + v, y)
        
exec' :: [(Dir, Int)] -> (Int, Int, Int)
exec' = foldl mv (0, 0, 0)
  where
    mv :: (Int, Int, Int) -> (Dir, Int) -> (Int, Int, Int)
    mv (x, y, aim) (dir, v) =
      case dir of
        Dn  -> (x, y, aim + v)
        Up  -> (x, y, aim - v)
        Fwd -> (x + v, y + aim * v, aim)

solve :: [[String]] -> Int
solve = uncurry (*) . exec . parse

solve' :: [[String]] -> Int
solve' ws = 
  let (x, y, aim) = exec' (parse ws)
  in x * y

readInt :: String -> Int
readInt s = read s
  
main :: IO ()
main = do
  text <- readFile "data2.txt"
  let ws = fmap words $ lines text
  print $ solve ws
  print $ solve' ws
 
