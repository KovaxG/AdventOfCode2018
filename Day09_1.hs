{-# LANGUAGE BangPatterns #-}
import Data.List.PointedList.Circular
import Data.Maybe

type Marble = Int
type Score = Int
type Board = PointedList Marble
type Player = Score
data GameState = GameState ![Player] !Board !Marble

instance Show GameState where
  show (GameState players board marble) = show players ++ show board

initialState :: Int -> GameState
initialState playerNumber = GameState players board 1
  where
    board = singleton 0
    players = replicate playerNumber 0

proceed :: GameState -> GameState
proceed !(GameState players board currentMarble) = GameState players' board' (currentMarble + 1)
  where
    currentPlayer = head players
    players' = tail players ++ [currentPlayer']

    (currentPlayer', board') =
      if mod currentMarble 23 == 0
        then extraCase (currentPlayer, board)
        else (currentPlayer, normalCase board)

    normalCase :: Board -> Board
    normalCase = insert currentMarble . moveN 2

    extraCase :: (Player, Board) -> (Player, Board)
    extraCase (player, board) = (addScore player, board')
      where
        addScore score = score + currentMarble + extraScore
        (board', extraScore) =
               (\b -> (fromJust $ delete b, _focus b))
               $ moveN (-7) board

playGame :: Int -> Int -> GameState
playGame marbles playerNumber = go startState
  where
    startState = initialState playerNumber
    go !state@(GameState _ _ currentMarble);
      | currentMarble == marbles = state
      | otherwise = go (proceed state)

problem :: Int -> Int -> Int
problem playerNumber marbles =
  maximum
  . players
  $ playGame marbles playerNumber
  where
    players (GameState ps _ _) = ps

main :: IO ()
main = do
  putStrLn "Running the program. Brace yourself."
  putStrLn . show $ problem players lastMarble
  putStrLn "Done! OMG it wasn't easy for me, phew."
  hurray <- getLine
  putStrLn $ "As you said, " ++ hurray
  where
    players = 459
    lastMarble = 7179000
