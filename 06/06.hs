import Data.List (nub)

allDifferent xs = nub xs == xs

signals :: Int -> String -> Int
signals k xs
    | allDifferent (take k xs) = k
signals k (_:xs) = 1 + signals k xs

main :: IO ()
main = do
    input <- readFile "./input.txt"

    putStrLn "first answer"
    print $ signals 4 input

    putStrLn "second answer"
    print $ signals 14 input