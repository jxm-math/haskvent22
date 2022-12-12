import Data.Char (ord)

import qualified Data.Sequence as Sq
import qualified Data.Set      as St

type Point = (Int, Int)
type Heights = [[Int]]

type Frontier = Sq.Seq (Point, Int)

initHeights :: [[Char]] -> Heights
initHeights = map (map aux)
    where
        aux 'S' = ord 'a'
        aux 'E' = ord 'z'
        aux c = ord c

find :: [[Char]] -> Char -> [Point]
find g c =
    [ (x, y)
        | x <- [0 .. length g - 1],
          y <- [0 .. length (head g) - 1],
          g!!x!!y == c
    ]

dst :: Heights -> Point -> Frontier -> St.Set Point -> Int
dst h target (((x, y), d) Sq.:<| rest) visited
    | (x, y) == target = d
    | otherwise        = dst h target frontier' visited'
    where
        new =
            [ (xx,yy)
                | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)],
                  let xx = x + dx,
                  let yy = y + dy,
                  0 <= xx && xx < length h && 0 <= yy && yy < length (head h),
                  (h!!xx!!yy) <= (h!!x!!y) + 1,
                  (xx,yy) `St.notMember` visited
            ]
        frontier' = rest Sq.>< Sq.fromList (zip new (repeat (d + 1)))
        visited' = foldr St.insert visited new

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let i = lines input
    let h = initHeights i
    let start = head $ find i 'S'
    let end   = head $ find i 'E'

    putStrLn "first answer"
    print $ dst h end (Sq.singleton (start, 0)) (St.singleton start)

    putStrLn "second answer"
    let starts = start : find i 'a'
    print $ dst h end (Sq.fromList $ zip starts (repeat 0)) (St.fromList starts)