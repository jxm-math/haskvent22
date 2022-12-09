import Data.List (nub)

type Pos = (Int,Int)
type Rope = [Pos]

updateHead :: Char -> Pos -> Pos
updateHead 'L' (a,b) = (a-1,b)
updateHead 'R' (a,b) = (a+1,b)
updateHead 'D' (a,b) = (a,b-1)
updateHead 'U' (a,b) = (a,b+1)

updateNext :: Pos -> Pos -> Pos
updateNext (a,b) (c,d)
    | abs i < 2 && abs j < 2 = (c,d)
    | otherwise              = (c + signum i, d + signum j)
    where
        i = a - c
        j = b - d

updateRope :: Char -> Rope -> Rope
updateRope c (h:ts) = new
    where new = updateHead c h : zipWith updateNext new ts

ropePath :: [Char] -> Rope -> [Rope]
ropePath cs init = total
    where total = init : zipWith updateRope cs total

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let rep = \s -> replicate (read $ drop 2 s) (head s)   -- "L 5" -> "LLLLL"
    let orders = concatMap rep (lines input)
    let path = ropePath orders (repeat (0,0))

    putStrLn "first answer"
    print $ length $ nub (map (!!1) path)

    putStrLn "second answer"
    print $ length $ nub (map (!!9) path)