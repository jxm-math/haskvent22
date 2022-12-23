import Data.Char (isDigit, isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S

type Valve = String
type FlowMap = M.Map Valve Int
type PathMap = M.Map Valve [Valve]
type DistMap = M.Map (Valve,Valve) Int

decode :: String -> (Valve, Int, [Valve])
decode s = (v,i,vs)
    where
        [s1,s2] = splitOn ";" s
        v = take 2 (drop 6 s1)
        i = read (filter isDigit s1)
        vs = splitOn ", " $ dropWhile (not . isUpper) s2

-- Floyd-Warshall
dmin (-1) k = k
dmin k (-1) = k
dmin k l = min k l
dsum (-1) k = -1
dsum k (-1) = -1
dsum k l = k + l

fw :: PathMap -> DistMap
fw pm =
    let vs = M.keys pm
        d0 = M.fromList [((v,w),k) | v <- vs,
                                     w <- vs,
                                     let k = if w `elem` pm M.! v
                                             then 1
                                             else -1]
        improve :: DistMap -> Valve -> DistMap
        improve d z =
            M.fromList [((v,w),k) | v <- vs,
                                    w <- vs,
                                    let k1 = d M.! (v,w),
                                    let k2 = dsum (d M.! (v,z)) (d M.! (z,w)),
                                    let k  = dmin k1 k2]
    in foldl improve d0 vs

-- best score for part 1
win :: FlowMap -> DistMap -> Valve -> Int -> Int
win fm dm = aux S.empty
    where
        -- consider only the valves worth opening
        flowVs = M.keys $ M.filter (> 0) fm
        -- best strategy in terms of opened valves, current valve
        -- and remaining time
        aux opened curr rem =
            let mayBeNext v = S.notMember v opened && dm M.! (curr, v) < rem - 1
                cands = filter mayBeNext flowVs
                -- gain of going to open a certain valve
                gain v = let newRem = rem - (dm M.! (curr, v)) - 1
                             currValveGain = (fm M.! v) * newRem
                             rest = aux (S.insert v opened) v newRem
                         in currValveGain + rest
            in if null cands then 0 else maximum (map gain cands)

-- best score for part 2
win2 :: FlowMap -> DistMap -> Valve -> Int -> Int
win2 fm dm init = aux S.empty (0,init) (0,init)
    where
        -- consider only the valves worth opening
        flowVs = M.keys $ M.filter (> 0) fm
        aux _ _ _ 0 = 0
        -- for this new sceneario, time has to decrease one by one.
        -- (d,v) means that an agent is going towards valve v and is at dist d.
        -- d == 1 means arrival; d == 0 means the valve is open, in which case
        -- the agent looks for new valves where to go.
        aux opened (d1,v1) (d2,v2) rem =
            let mayBeNext c v = S.notMember v opened && dm M.! (c, v) < rem - 1
                -- possibilities for first agent
                next1 = if d1 == 0
                        then [(dm M.! (v1, v),v) | v <- flowVs, mayBeNext v1 v]
                        else [(d1-1,v1)]
                -- possibilities for second agent
                next2 = if d2 == 0
                        then [(dm M.! (v2, v),v) | v <- flowVs, mayBeNext v2 v]
                        else [(d2-1,v2)]
                -- if no available valves, remain at place until the end
                next'1 = if null next1 then [(-1,v1)] else next1
                next'2 = if null next2 then [(-1,v2)] else next2
                -- update open valves
                opened1 = if d1 == 0 then S.insert v1 opened  else opened
                opened2 = if d2 == 0 then S.insert v2 opened1 else opened1
                -- update flow - avoid opening twice the same valve
                flow1 = 0     + if d1 == 0 && S.notMember v1 opened
                                then fm M.! v1
                                else 0
                flow2 = flow1 + if d2 == 0 && S.notMember v2 opened1
                                then fm M.! v2
                                else 0
                wins = [aux opened2 g1 g2 (rem - 1) |
                            g1 <- next'1,
                            g2 <- next'2,
                            -- discard both agents going to the same valve
                            snd g1 /= snd g2,
                            -- if in the init, avoid symmetric strategies -
                            -- this reduces computing time to half
                            d1 /= 0 || d2 /= 0 || v1 /= v2 || snd g1 < snd g2]
            in flow2 * rem + if null wins then 0 else maximum wins

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let structure = map decode $ lines input
    let (fm,pm) = foldl
            (\(f,p) (v,i,vs) -> (M.insert v i f, M.insert v vs p))
            (M.empty, M.empty)
            structure
    let dm = fw pm

    putStrLn "first answer"
    print $ win  fm dm "AA" 30

    putStrLn "second answer"
    print $ win2 fm dm "AA" 26
