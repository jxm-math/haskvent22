import Data.List.Split (splitOn)

decode :: String -> [[Int]]
decode = map (map read . splitOn "-") . splitOn ","

fullyContains :: [[Int]] -> Bool
fullyContains [[a,b],[c,d]] = (a <= c && b >= d) || (a >= c && b <= d)

overlap :: [[Int]] -> Bool
overlap [[a,b],[c,d]] = not (c > b || d < a)

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let assigns = lines input

    putStrLn "first answer"
    print $ length . filter (fullyContains . decode)  $ assigns

    putStrLn "second answer"
    print $ length . filter (overlap . decode)  $ assigns