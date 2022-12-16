import Data.Char (isDigit)
import Data.List (find, nub)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust, mapMaybe)

type Point = (Int, Int)
type SB = (Point, Point)
type Range = (Int, Int)

decode :: String -> SB
decode = (\[a,b,c,d] -> ((a,b),(c,d)))
    . map read . filter (not . null)
    . splitWhen (\x -> not (isDigit x) && x /= '-')

manhattan :: Point -> Point -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

-- the range in which a circle of a source-beacon pair
-- overlaps the line of height y
rangeForY :: Int -> SB -> Maybe Range
rangeForY y (p@(a,b),q@(c,d))
    | dist <= rad = Just (a - (rad-dist), a + (rad-dist))
    | otherwise = Nothing
    where
        rad = manhattan p q
        dist = abs (y-b)

-- the process of merging ranges so that the output remains
-- a list of ordered and disjoint ranges
mergeRanges :: Range -> [Range] -> [Range]
mergeRanges r [] = [r]
mergeRanges s@(x1,x2) (r@(y1,y2):rs)
    | x2 < y1 - 1 = s : r : rs
    | y2 < x1 - 1 = r : mergeRanges s rs
    | otherwise = mergeRanges (min x1 y1, max x2 y2) rs

-- the full list of covered ranges for a given height
rangesForY :: Int -> [SB] -> [Range]
rangesForY y sbs =
    let allRanges = mapMaybe (rangeForY y) sbs
    in foldl (flip mergeRanges) [] allRanges

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let sbs = map decode (lines input)

    putStrLn "first answer"
    let y0 = 2000000
    let nbs = (length . nub) [c | (_,(c,d)) <- sbs, d == y0]
    print $ sum (map (\ (x1, x2) -> x2 - x1 + 1) $ rangesForY y0 sbs) - nbs

    putStrLn "second answer"
    let (y, rs) = head [(y, rs) |
            y <- enumFromTo 0 4000000,
            let rs = rangesForY y sbs,
            not $ any (\(x1,x2) -> x1 <= 0 && x2 >= 4000000) rs]
    let x = fst (fromJust $ find (\ (x0, _) -> x0 > 0) rs) - 1
    print $ 4000000 * x + y