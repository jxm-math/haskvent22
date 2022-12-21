import Data.List.Split (splitOn)
import qualified Data.Set as S

type Cube = (Int,Int,Int)
type Space = S.Set Cube

decode :: String -> Cube
decode = (\[a,b,c] -> (a,b,c)) . map read . splitOn ","

dirs = [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)]

move (a,b,c) (i,j,k) = (a+i,b+j,c+k)

-- the surface of a cube inside a set of cubes - it checks its neighbors
cubeSurf :: Cube -> Space -> Int
cubeSurf c l = length [() | d <- dirs, S.notMember (move c d) l]

-- whole surface of a set of cubes
surf :: Space -> Int
surf l = sum $ map (`cubeSurf` l) (S.elems l)

-- connected component - depth first search
comp :: (Cube -> Bool) -> Cube -> Space
comp p c = aux (S.singleton c) c
    where aux space c =
            -- which adjacent cubes are inside the set but still not visited
            let adjs = [c' | d <- dirs,
                             let c' = move c d,
                             p c',
                             S.notMember c' space]
            -- keep looking in each of them
            in foldl aux (S.insert c space) adjs

-- list of connected components for a list of cubes
comps :: (Cube -> Bool) -> [Cube] -> [Space]
comps p = foldl addComp []
    where addComp ss c = if any (S.member c) ss then ss else comp p c : ss

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let cubes = map decode $ lines input
    let lava = S.fromList cubes

    putStrLn "first answer"
    print $ surf lava

    putStrLn "second answer"

    -- bounds of a box containing the lava with connected exterior
    let xs = map (\(a,b,c) -> a) cubes
    let x0 = minimum xs - 1
    let x1 = maximum xs + 1
    let ys = map (\(a,b,c) -> b) cubes
    let y0 = minimum ys - 1
    let y1 = maximum ys + 1
    let zs = map (\(a,b,c) -> c) cubes
    let z0 = minimum zs - 1
    let z1 = maximum zs + 1

    -- predicate for being in the box but not in the lava
    let ext (a,b,c) =  x0 <= a && a <= x1
                    && y0 <= b && b <= y1
                    && z0 <= c && c <= z1
                    && S.notMember (a,b,c) lava

    -- list of positions in the box
    let box = [(a,b,c) | a <- enumFromTo x0 x1,
                         b <- enumFromTo y0 y1,
                         c <- enumFromTo z0 z1]

    -- connected components outside the lava; filtering out the exterior one
    let extComps = comps ext $ filter ext box
    let bubbles = filter (S.notMember (x0,y0,z0)) extComps

    print $ surf lava - sum (map surf bubbles)