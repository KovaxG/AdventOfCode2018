import Common
import Data.Either.Extra
import qualified Data.Matrix as Matrix
import qualified Data.Map.Strict as Map
import Data.List
import Debug.Trace
import Text.Parsec hiding (count)

data Id = Id Int | X | N deriving (Show, Eq)
data Claim = Claim Id (Int, Int) (Int, Int) deriving (Show)

add :: Id -> Id -> Id
add N (Id a) = Id a
add (Id a) N = Id a
add _ _ = X

parseClaim :: String -> Claim
parseClaim = fromRight undefined . parse line ""
  where
    line = do
      char '#'
      n <- read <$> many digit
      spaces
      char '@'
      spaces
      sx <- read <$> many digit
      char ','
      sy <- read <$> many digit
      char ':'
      spaces
      w <- read <$> many digit
      char 'x'
      h <- read <$> many digit
      return $ Claim (Id n) (sx, sy) (w, h)

calc :: [String] -> Int
calc = count X . map snd . Map.toList . foldl rule Map.empty . map parseClaim
  where
    rule :: Map.Map (Int, Int) Id -> Claim -> Map.Map (Int, Int) Id
    rule points (Claim i (sx, sy) (w, h)) = foldr (\p -> Map.insertWith add p i) points indexes
      where
        xs = map (\w -> w + sx) [1..w]
        ys = map (\w -> w + sy) [1..h]
        indexes = [(x, y) | x <- xs, y <- ys]

main :: IO ()
main = do
  bla <- loadDo calc "Day03_1.input"
  putStrLn $ show bla
