import Data.List.Split (splitWhen)
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let elvesChunks = splitWhen null (lines input)
    let elvesTotals = map (sum . map read) elvesChunks

    putStrLn "first answer"
    putStrLn . show $ maximum elvesTotals

    let sortedElves = sort elvesTotals

    putStrLn "second answer"
    putStrLn . show . sum . take 3 . reverse $ sortedElves