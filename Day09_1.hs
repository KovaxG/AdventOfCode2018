{-# LANGUAGE BangPatterns #-}
import Data.List.Zipper

type Marble = Int
type Score = Int
type Board = Zipper Marble
data Player = Player Int Score
data GameState = GameState ![Player] !Board !Marble

instance Show Player where
  show (Player id score) = "P" ++ show id ++ ": " ++ show score

instance Show GameState where
  show (GameState players board marble) = show players ++ show (toList board)

initialState :: Int -> GameState
initialState playerNumber = GameState players board 1
  where
    board = fromList [0]
    players = map (\i -> Player i 0) [1 .. playerNumber]

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
    normalCase =
      insert currentMarble
      . clockwise
      . clockwise

    extraCase :: (Player, Board) -> (Player, Board)
    extraCase (player, board) = (addScore player, board')
      where
        addScore (Player id score) = Player id (score + currentMarble + extraScore)
        (board', extraScore) =
               (\b -> (delete b, cursor b))
               . counterClockwise
               . counterClockwise
               . counterClockwise
               . counterClockwise
               . counterClockwise
               . counterClockwise
               $ counterClockwise board

clockwise :: Zipper a -> Zipper a
clockwise z
  | endp (right z) = start z
  | otherwise = right z

counterClockwise :: Zipper a -> Zipper a
counterClockwise z
  | beginp (left z)  && beginp z = left $ end z
  | otherwise = left z

playGame :: Int -> Int -> GameState
playGame marbles playerNumber = go startState
  where
    startState = initialState playerNumber
    go state@(GameState _ _ currentMarble)
      | currentMarble == marbles = state
      | otherwise = go (proceed state)

problem :: Int -> Int -> Int
problem playerNumber marbles =
  maximum
  . map scoreOf
  . players
  $ playGame marbles playerNumber
  where
    players (GameState ps _ _) = ps
    scoreOf (Player _ score) = score

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
