import Data.Char (digitToInt)
import Data.List (transpose)

-- Traverses a list of integers and indicates whether the trees are visible
-- in this direction
-- e.g. areVisible [1,4,3,5,2] = [True,True,False,True,False]
-- aux keeps the height of the tallest tree seen so far
areVisible :: [Int] -> [Bool]
areVisible = aux (-1)
    where
        aux :: Int -> [Int] -> [Bool]
        aux _ [] = []
        aux k (x:xs) = (x > k) : aux (max x k) xs

-- Traverses a list of integers and indicates the view distances towards
-- the init of the list
-- e.g. viewDistance [1,4,3,5,2] = [0,1,1,3,1]
-- aux keeps a list [(Int,Int)] with a sequence of decreasing trees, along
-- with their positions, that may be the limit of view for oncoming trees.
-- For instance, if we have seen the following trees
--
--        *
--        x       *
--      x x   x   x   *
--      x x x x   x x x
--      x x x x x x x x
-- then the sequence would be [(5,1),(4,5),(3,6)] representing the trees
-- marked with (*)
viewDistance :: [Int] -> [Int]
viewDistance xs = aux [] (zip xs [0..])
    where
        aux :: [(Int,Int)] -> [(Int,Int)] -> [Int]
        aux _ [] = []
        aux bs (x:xs) = let (d,bs') = comp x bs in d : aux bs' xs
            where
                comp (h,i) [] = (i,[(h,i)])
                comp (h,i) t@((k,j):bs) = if h <= k
                    then (i-j,(h,i):t)
                    else comp (h,i) bs

-- generalizing traversals for the whole grid and all directions
fromL :: ([Int] -> [a]) -> [[Int]] -> [[a]]
fromL = map

fromT :: ([Int] -> [a]) -> [[Int]] -> [[a]]
fromT t = transpose . fromL t . transpose

fromR :: ([Int] -> [a]) -> [[Int]] -> [[a]]
fromR t = map reverse . fromL t . map reverse

fromB :: ([Int] -> [a]) -> [[Int]] -> [[a]]
fromB t = transpose . fromR t . transpose

dirs = [fromL,fromT,fromR,fromB]

-- double zipWith for pointwise operations in grid
dz = zipWith . zipWith

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let grid = map (map digitToInt) $ lines input

    putStrLn "first answer"
    let vis = foldl1 (dz (||)) (map (\t -> t areVisible grid) dirs)
    print $ sum $ map (sum . map (\x -> if x then 1 else 0)) vis

    putStrLn "second answer"
    let scn = foldl1 (dz (*)) (map (\t -> t viewDistance grid) dirs)
    print $ maximum $ map maximum scn