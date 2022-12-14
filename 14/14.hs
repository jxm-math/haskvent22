import Data.List.Split (splitOn)

type Point = (Int, Int)
type Path = [Point]
type Grid = [[Bool]]

decode :: String -> Path
decode = map ((\[x,y] -> (x,y)) . map read . splitOn ",") . splitOn " -> "

between :: Point -> Point -> Point -> Bool
between (x0,y0) (x1,y1) (x,y) =
    (x==x0 && x==x1 && mid y0 y y1) || (y==y0 && y==y1 && mid x0 x x1)
    where mid a b c = (a <= b && b <= c) || (c <= b && b <= a)

inPath :: Path -> Point -> Bool
inPath (q:r:ps) p = between q r p || inPath (r:ps) p
inPath [q] p = False -- should have already matched

sx = 500

-- Creates a grid from a list of paths.
-- It tries to be as small as possible, so an integer denoting a lateral shift
-- comes in the output.
-- Dimensions of the grid are computed so that a unit of sand getting out of
-- the grid would fall forever.
-- The boolean input 'extended' modelles the different behavior between
-- part 1 and 2.
-- For part 2, width has to be expanded so that a cone of sand may enter
-- without touching the lateral limits.
createGrid :: Bool -> [Path] -> (Int, Grid)
createGrid extended hs =
    let ps = concat hs
        h1 = maximum $ map snd ps
        w0 = if extended then sx - h1 - 3 else minimum $ map fst ps
        w1 = if extended then sx + h1 + 3 else maximum $ map fst ps
        rock x y = any (\h -> inPath h (x + w0, y)) hs
        w = w1 - w0
        g0 = [[ rock x y | x <- [0..w] ] | y <- [0..h1] ]
        g = if extended then g0 ++ map (replicate (w+1)) [False, True] else g0
    in (w0, g)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 x (y:ys) = x : ys
replaceAt n x (y:ys) = y : replaceAt (n-1) x ys


-- The fall of a unit of sand.
-- The boolean in the output indicates whether the sand has effectively been
-- placed somewhere.
fall :: Point -> Grid -> (Bool, Grid)
fall (a,b) g
    | g!!a!!b               = (False, g)
    | not (inside (1, 0))   = (False, g)
    | not (g!!(a+1)!!b)     = fall (a+1,b) g
    | not (inside (1,-1))   = (False, g)
    | not (g!!(a+1)!!(b-1)) = fall (a+1,b-1) g
    | not (inside (1, 1))   = (False, g)
    | not (g!!(a+1)!!(b+1)) = fall (a+1,b+1) g
    | otherwise             = (True, place (a,b) g)
    where
        inside (x,y) = a+x < length g && 0 <= b+y && b+y < length (head g)
        place (a,b) g = replaceAt a (replaceAt b True (g!!a)) g

unitsOfSand :: Grid -> Point -> Int
unitsOfSand grid source =
    let ev = iterate (fall source . snd) (True, grid)
    in length (takeWhile fst ev) - 1

-- print util
printGrid :: Grid -> IO ()
printGrid = mapM_ (print . map pix)
    where pix b = if b then '#' else '.'

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let hs = map decode (lines input)

    putStrLn "first answer"
    let (shift, grid) = createGrid False hs
    -- printGrid grid
    let source = (0, sx-shift)
    print $ unitsOfSand grid source

    putStrLn "second answer"
    let (shift, grid) = createGrid True hs
    -- printGrid grid
    let source = (0, sx-shift)
    print $ unitsOfSand grid source