import Data.List (elemIndex, sort)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)

data Item = Number Int | List [Item] deriving (Eq)

instance Ord Item where
    Number i <= Number j = i <= j
    List xs  <= List ys  = xs <= ys
    Number i <= List ys  = [Number i] <= ys
    List xs  <= Number j = xs <= [Number j]

parse :: String -> Item
parse = head . head . fst . foldr aux ([[]],[])
    where
        aux :: Char -> ([[Item]],[Char]) -> ([[Item]],[Char])
        aux ']' (s,_) = ([]:s,[])
        aux '[' t = let (x:y:ys,_) = moveInts t in ((List x : y) : ys, [])
        aux ',' t = moveInts t
        aux c (s,is) = (s,c:is)
        moveInts t@(x:xs,is) = if null is
            then t
            else ((Number (read is) : x) : xs, [])

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let chunks = map (map parse) $ splitWhen null (lines input)

    putStrLn "first answer"
    print $ sum [k | ([x,y], k) <- zip chunks [1..], x <= y]

    putStrLn "second answer"
    let new = [parse "[[2]]", parse "[[6]]"]
    let total = new ++ concat chunks
    let sorted = sort total
    print $ product $ map (\x -> fromJust (elemIndex x sorted) + 1) new