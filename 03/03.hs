import Data.Char (isLower, ord)
import Data.List (intersect)

-- common character in a list of strings
common :: [String] -> Char
common ss = head $ foldl1 intersect ss

-- common character when dividing a string in half
middleCommon :: String -> Char
middleCommon xs = common [as,bs]
    where
        w  = length xs `div` 2
        as = take w xs
        bs = drop w xs

-- priority of each character
priority :: Char -> Int
priority x
    | isLower x = ord x - ord 'a' + 1
    | otherwise = ord x - ord 'A' + 27

-- utility to make trios from a list
trios :: [a] -> [[a]]
trios [] = []
trios (a:b:c:xs) = [a,b,c] : trios xs

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let rucksacks = lines input
    let rucksackTrios = trios rucksacks

    putStrLn "first answer"
    print $ sum . map (priority . middleCommon) $ rucksacks

    putStrLn "second answer"
    print $ sum . map (priority . common) $ rucksackTrios