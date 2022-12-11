import Data.List (sort)
import Data.List.Split (splitOn, splitWhen)

data Monkey = Monkey { items :: [Int]
                     , wait :: [Int]
                     , thrown :: Int
                     , op :: Int -> Int
                     , test :: Int
                     , ifTrue :: Int
                     , ifFalse :: Int
                     }

type Pack = [Monkey]
type WorryTrans = Int -> Int

decodeOp :: String -> Int -> Int
decodeOp x k = op (val h1 k) (val h2 k)
    where
        [h1,o,h2] = words x
        op = if o == "+" then (+) else (*)
        val s v = if s == "old" then v else read s

decode :: [String] -> Monkey
decode xs = Monkey { items = map read . splitOn "," $ drop 18 (xs!!1)
                   , wait = []
                   , thrown = 0
                   , op =  decodeOp $ drop 19 (xs!!2)
                   , test =    read $ drop 21 (xs!!3)
                   , ifTrue =  read $ drop 29 (xs!!4)
                   , ifFalse = read $ drop 30 (xs!!5)
                   }

allItems :: Monkey -> [Int]
allItems m = items m ++ reverse (wait m)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 x (y:ys) = x : ys
replaceAt n x (y:ys) = y : replaceAt (n-1) x ys

inspect :: WorryTrans -> Monkey -> Int -> (Int, Int)
inspect t m k = let w = t (op m k)
                in (w, if w `mod` test m == 0 then ifTrue m else ifFalse m)

throw :: (Int, Int) -> Pack -> Pack
throw (i,k) h = replaceAt k (let m = h!!k in m {wait = i : wait m}) h

throwAllItems :: WorryTrans -> Pack -> Int -> Pack
throwAllItems t h k =
    let m = h!!k;
        list = allItems m;
        outputs = map (inspect t m) list;
        newMonkey = m {items = [], wait = [], thrown = length list + thrown m}
    in foldl (flip throw) (replaceAt k newMonkey h) outputs

rnd :: WorryTrans -> Pack -> Pack
rnd t h = foldl (throwAllItems t) h (enumFromTo 0 (length h - 1))

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let monkeyChunks = splitWhen null (lines input)
    let herd = map decode monkeyChunks

    putStrLn "first answer"
    let herdEv = iterate (rnd (`div` 3)) herd
    let inspects = map thrown $ herdEv!!20
    print $ let (a:b:xs) = reverse $ sort inspects in a * b

    putStrLn "second answer"
    let md = foldl1 lcm $ map test herd
    let herdEv = iterate (rnd (`mod` md)) herd
    let inspects = map thrown $ herdEv!!10000
    print $ let (a:b:xs) = reverse $ sort inspects in a * b