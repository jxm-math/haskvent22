snafuVal :: Char -> Int
snafuVal '2' =  2
snafuVal '1' =  1
snafuVal '0' =  0
snafuVal '-' = -1
snafuVal '=' = -2

snafuToDecimal :: String -> Int
snafuToDecimal = aux . reverse
    where
        aux [] = 0
        aux (c:cs) = snafuVal c + 5 * aux cs

decimalToRevSnafu :: Int -> String
decimalToRevSnafu 0 = ""
decimalToRevSnafu n =
    let r = n `mod` 5
        aux 0 = ('0', n `div` 5)
        aux 1 = ('1', (n-1) `div` 5)
        aux 2 = ('2', (n-2) `div` 5)
        aux 3 = ('=', (n+2) `div` 5)
        aux 4 = ('-', (n+1) `div` 5)
        (c,m) = aux r
    in c : decimalToRevSnafu m

decimalToSnafu :: Int -> String
decimalToSnafu = reverse . decimalToRevSnafu

main :: IO ()
main = do
    input <- readFile "./input.txt"
    print $ decimalToSnafu $ sum $ map snafuToDecimal $ lines input