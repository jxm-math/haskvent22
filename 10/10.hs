import Data.List.Split (chunksOf)

decode :: String -> [Int]
decode "noop" = [0]
decode s = [0, read (drop 5 s)]

lit :: [Int] -> [Bool]
lit rs = zipWith (\a b -> abs (a-b) <= 1) rs [0..]

pix b = if b then '#' else '.'

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let orders = concatMap decode (lines input)
    let registers = scanl (+) 1 orders

    putStrLn "first answer"
    let signals = map head $ chunksOf 40 $ drop 19 registers
    print $ sum $ zipWith (*) signals [20,60..]

    putStrLn "second answer"
    let screen = map (map pix . lit) $ chunksOf 40 registers
    mapM_ print screen