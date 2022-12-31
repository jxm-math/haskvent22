import Data.Char (isDigit)
import Data.List.Split (splitWhen)
import Data.Maybe (mapMaybe)

type Material = Int -- 0, 1, 2, 3

type Goods  = [Int] -- or robots
type Robots = Goods

type Blueprint = [Goods]

decode :: String -> Blueprint
decode = (\[a,b,c,d,e,f,g] -> [[b,0,0,0],
                               [c,0,0,0],
                               [d,e,0,0],
                               [f,0,g,0]])
    . map read . filter (not . null)
    . splitWhen (not . isDigit)

type Context = (Goods, Robots, Int)

-- A first approach for this problem is to consider in each step
-- whether to spend or not.
-- However, this is very inneficient in terms of branching, even with some
-- prune heuristics.
-- One may consider cases in which for time `t` one does not spend and for `t+1`
-- one spends in some robot that could habe been acquired in time `t`.
-- These strategies are clearly not optimal.
-- We will proceed as follows: we will plan which robot to acquire next.
-- For each robot, we will check whether it will affordable by just waiting,
-- and if so, we will compute how much time will we have to wait.
-- This way, branching is still over all the acquirable robots,
-- but the time steps are bigger.
goFor :: Blueprint -> Context -> Material -> Maybe Context
goFor bp (g,r,t) mat
    | or $ zw3 (\g r c -> g < c && r == 0) = Nothing
    | otherwise = let tt = 1 + max 0 (maximum $ zw3 (\g r c ->
                        ceiling $ fromIntegral (c-g) / fromIntegral r))
                  in if tt > t
                     then Nothing
                     else Just (zw3 (\g r c -> g+tt*r-c),
                                zipWith (+) r (inc mat),
                                t-tt)
    where
        c = bp!!mat
        inc mat = map (\i -> if i == mat then 1 else 0) [0..]
        zw3 f = zipWith3 f g r c

-- maximum number of geodes achievable in a given time
geodes :: Int -> Blueprint -> Int
geodes time bp = geodesFrom bp ([0,0,0,0],[1,0,0,0], time)
    where
        geodesFrom :: Blueprint -> Context -> Int
        geodesFrom _ (g, _, 0) = g!!3
        geodesFrom bp ct@(g,r,t) =
            let pos = mapMaybe (goFor bp ct) [0..3]
            in max
                (g!!3 + r!!3 * t)
                (if null pos then 0 else maximum $ map (geodesFrom bp) pos)

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let bps = map decode (lines input)

    -- quite long computing time
    putStrLn "first answer"
    let gains1 = map (geodes 24) bps
    print $ sum $ zipWith (*) gains1 [1..]

    -- unaffordable computing time
    putStrLn "second answer"
    let gains2 = map (geodes 32) (take 3 bps)
    print $ product gains2