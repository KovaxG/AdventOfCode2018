import Debug.Trace

type Player = Int
type Marble = Int
type Score = Int
data Game = Game Marble Marble [Marble] [(Player, Score)] deriving (Show)

myShow :: Game -> String
myShow (Game _ current marbles players) =
  show (last players) ++ " - " ++ concat marblesString
  where
    marblesString = map (\m ->
      if m == current
      then "(" ++ show m ++ ") "
      else show m ++ " ") (rotateToZero allMarbles)
    allMarbles = current : marbles
    rotateToZero ms@(0:_) = ms
    rotateToZero ms = rotateToZero (rotateCW ms)

startPlayers :: Int -> [(Player, Score)]
startPlayers n = map (\p -> (p,0)) [1..n]

start = Game 1 1 [0] $ startPlayers 10

next :: Game -> Game
next (Game mn current marbles players) = Game mnp nextMarble newMarbles newPlayers
  where
    mnp = mn + 1
    newPlayers = tail players ++ [newPlayer]
    (playerId, score) = head players
    newPlayer = (playerId, score + extraScore)
    (nextMarble, newMarbles, extraScore) =
      if mnp `mod` 23 == 0
      then extraCase
      else normalCase

    normalCase :: (Marble, [Marble], Score)
    normalCase =
      let nextMarble = mnp
          newMarbles = repeatN (length marbles) rotateCW (current : marbles)
      in (nextMarble, newMarbles, 0)

    extraCase :: (Marble, [Marble], Score)
    extraCase =
      let extraScore = mnp
          sevenBack = repeatN 8 rotateCW (current : marbles)
          asd = head sevenBack -- by evaluating this the algorithm dies
          nextMarble = last $ take 2 sevenBack
          rest = drop 2 sevenBack
      in trace (show mnp ++ " " ++ show (extraScore)) (nextMarble, rest, extraScore)

rotateCW :: [a] -> [a]
rotateCW [] = []
rotateCW as = last as : init as

repeatN :: Int -> (a -> a) -> a -> a
repeatN (-1) _ a = a
repeatN 0 _ a = a
repeatN n f z = foldl (\a _ -> f a) z [1..n]

getMaxScore :: Game -> Score
getMaxScore (Game _ _ _ players) = maximum $ map snd players

main :: IO ()
main = do
  let endGame = repeatN 1619 next start
  putStrLn . show $ getMaxScore endGame
