data Hand = Rock | Paper | Scissors
data Game = Lose | Draw | Win

-- result for the second player
match :: Hand -> Hand -> Game
match Rock Scissors     = Lose
match Scissors Paper    = Lose
match Paper Rock        = Lose
match Scissors Rock     = Win
match Paper Scissors    = Win
match Rock Paper        = Win
match Rock Rock         = Draw
match Scissors Scissors = Draw
match Paper Paper       = Draw

score :: Hand -> Hand -> Int
score h k = reward (match h k) + bonus k
    where
        reward Lose = 0
        reward Draw = 3
        reward Win  = 6
        bonus Rock     = 1
        bonus Paper    = 2
        bonus Scissors = 3

-- character interpretation according to problem 1
decode1 :: Char -> Hand
decode1 'A' = Rock
decode1 'B' = Paper
decode1 'C' = Scissors
decode1 'X' = Rock
decode1 'Y' = Paper
decode1 'Z' = Scissors

-- full score of two chars according to problem 1
fullScore1 :: (Char, Char) -> Int
fullScore1 (a,b) = score (decode1 a) (decode1 b)

-- character interpretation according to problem 2
decode2 :: Char -> Game
decode2 'X' = Lose
decode2 'Y' = Draw
decode2 'Z' = Win

-- hand that the second player should play to achieve the desired game result
guess :: Hand -> Game -> Hand
guess h Draw = h
guess Rock     Win  = Paper
guess Paper    Win  = Scissors
guess Scissors Win  = Rock
guess Rock     Lose = Scissors
guess Paper    Lose = Rock
guess Scissors Lose = Paper

-- full score of two chars according to problem 2
fullScore2 :: (Char, Char) -> Int
fullScore2 (a,b) = let h = decode1 a in score h (guess h (decode2 b))

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let trials = map (\l -> (l!!0,l!!2)) (lines input)

    putStrLn "first answer"
    print $ sum . map fullScore1 $ trials

    putStrLn "second answer"
    print $ sum . map fullScore2 $ trials


