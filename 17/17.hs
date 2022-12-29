import Data.Bits
import Data.List (foldl')

-- each of the five rocks
data Rock = Hiphen | Plus | Corner | Vert | Square
rocks = [Hiphen, Plus, Corner, Vert, Square] ++ rocks

type Pos   = (Int, Int)
-- Positioned rocks.
-- Each positioned rock has an anchor in the bottom left corner.
type PRock = (Pos, Rock)
procks = zip (repeat (-4,2)) rocks -- all rocks starting at (-4,2)

type Jets = String

-- coords of a rock
coords :: Rock -> [Pos]
coords Hiphen = [(0,0), (0,1), (0,2), (0,3)]
coords Plus   = [(-1,0), (-1,1), (-1,2), (-2,1), (0,1)]
coords Corner = [(0,0), (0,1), (0,2), (-1,2), (-2,2)]
coords Vert   = [(0,0), (-1,0), (-2,0), (-3,0)]
coords Square = [(0,0), (0,1), (-1,0), (-1,1)]

-- coords of a positioned rock
pcoords :: PRock -> [Pos]
pcoords ((i,j), rock) = map (\(x,y) -> (x+i,y+j)) (coords rock)

-- moving a oositioned rock
left  ((i,j), rock) = ((i,j-1), rock)
right ((i,j), rock) = ((i,j+1), rock)
down  ((i,j), rock) = ((i+1,j), rock)

-- Chamber will be represented as follows:
-- Each row may be thought as an array of seven booleans in the y-coordinate,
-- but will be handled as an integer with bit operations to improve performance.
-- A chamber will be a list of rows, but since it may become huge, it will be
-- split in an upper section and a count (depth) of the rows below that section.
-- The altitude is managed by the x-coordinate: 0 is the topmost row, and above
-- that row, x becomes negative. This way a section is represented as a list,
-- where the head is the topmost row and the other elements are the rows below.
type Row = Int
type Section = [Row]
type Chamber = (Section, Int)
base :: Chamber
base = ([],0) -- floor

overlaps :: PRock -> Section -> Bool
overlaps prock sect = any over (pcoords prock)
    where over (x,y) = y < 0                          -- overlaps the left wall
                    || y > 6                          -- or the right wall
                    || x >= length sect               -- or the floor / full row
                    || (x >= 0 && testBit (sect!!x) y)-- or a rock inside

fill :: Pos -> Section -> Section
fill (x,y) sect = rep x (setBit (sect!!x) y) sect
    where -- list replacement
        rep 0 x (y:ys) = x : ys
        rep n x (y:ys) = y : rep (n-1) x ys

-- Fill a chamber with a new positioned rock.
-- It has some intricacies because new rows may be needed for the chamber.
-- Moreover, there is a cut process to consider a smaller section.
-- If a row is seen to be full, there is no need to keep the elements below;
-- there are other configurations where it may be ensured that no rock may pass,
-- but it's not worth the complexity.
solidify :: PRock -> Chamber -> Chamber
solidify prock (sect,d) = newChamber
    where
        pos = pcoords prock
        newRows = abs $ min 0 (minimum $ map fst pos)
        bigSect = replicate newRows zeroBits ++ sect
        newSect = foldr (fill . (\(a,b) -> (a + newRows, b))) bigSect pos
        fullRows = [x' | (x,_) <- pos, let x' = x + newRows, newSect!!x' == 127]
        newChamber = if null fullRows
                     then (newSect,d)
                     else let cut = minimum fullRows
                          in (take cut newSect, d + length newSect - cut)

-- Full fall process.
-- Keeps a list of jets to keep track for next falls
fall :: PRock -> (Chamber,Jets) -> (Chamber,Jets)
-- force `d` computation. Otherwise huge uncomputed sums cause memory leak.
fall prock (chamber@(sect,d), j:js) = d `seq` result
    where
        pr1  = (if j == '<' then left else right) prock
        pr1' = if overlaps pr1 sect then prock else pr1
        pr2  = down pr1'
        downCrash = overlaps pr2 sect
        result = if downCrash
                 then (solidify pr1' chamber, js)
                 else fall pr2 (chamber, js)

-- auxiliary representation
repr :: Chamber -> IO ()
repr (sect,d) = do {
        mapM_ (putStrLn . (\l -> "|" ++ pix l ++ "|")) sect;
        putStrLn "+-------+";
        putStrLn $ "Depth: " ++ show d;
        putStrLn ""; }
    where
        pix l = map (\i -> if testBit l i then '#' else '.') [0..6]

-- height of a chamber
height :: Chamber -> Int
height (sect,d) = length sect + d

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let jets = input ++ jets
    let init = (base, jets)
    -- strict foldl so that each step is computed
    let ev k = foldl' (flip fall) init (take k procks)

    putStrLn "first answer"
    print $ height (fst (ev 2022))

    -- no memory overflow, but still unaffordable computing time
    putStrLn "second answer"
    print $ height (fst (ev 1000000000000))