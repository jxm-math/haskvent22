import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Split (splitWhen)

-- "move 3 from 4 to 5" -> [3,4,5]
decodeMove :: String -> [Int]
decodeMove = map read . filter (not . null) . splitWhen (not . isDigit)

type Stack = [Char]
type Ship = [Stack]
type Action = Ship -> Ship

-- replace n-th element of a list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 x (y:ys) = x : ys
replaceAt n x (y:ys) = y : replaceAt (n-1) x ys

-- move n crates at once from one stack to another
moveInStack :: Int -> Stack -> Stack -> (Stack, Stack)
moveInStack k xs ys = (drop k xs, take k xs ++ ys)

-- the same movement, but inside the ship
moveInShip :: Int -> Int -> Int -> Action
moveInShip k i j s = replaceAt i a (replaceAt j b s)
    where (a,b) = moveInStack k (s!!i) (s!!j)

-- execution of order with CrateMover 9000
order1 :: String -> Action
order1 o s = iterate (moveInShip 1 (i-1) (j-1)) s !! n
    where [n,i,j] = decodeMove o

-- execution of order with CrateMover 9001
order2 :: String -> Action
order2 o = moveInShip n (i-1) (j-1)
    where [n,i,j] = decodeMove o

-- decoding of the initial ship configuration
decodeShip :: [String] -> Ship
decodeShip = map (filter (/= '_')) . transpose . map dec
    where
        dec [] = []
        dec ('[': c :']':' ':cs) =  c  : dec cs
        dec (' ':' ':' ':' ':cs) = '_' : dec cs
        dec ('[': c :']':cs)     =  c  : dec cs
        dec (' ':' ':' ':cs)     = '_' : dec cs

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let [shipConfig, moves] = splitWhen null (lines input)
    let initShip = decodeShip $ init shipConfig

    putStrLn "first answer"
    print $ map head $ foldl (flip order1) initShip moves

    putStrLn "second answer"
    print $ map head $ foldl (flip order2) initShip moves