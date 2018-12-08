import Common
import Data.Either.Extra
import qualified Data.Matrix as Matrix
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Data.List
import Text.Parsec

type Coord = (Int, Int)

dist :: Coord -> Coord -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseCoord :: String -> Coord
parseCoord = fromRight undefined . parse rule ""
  where
    rule = do
      x <- read <$> many digit
      spaces
      char ','
      spaces
      y <- read <$> many digit
      return (x, y)

allDistances :: [Coord] -> Coord -> Int
allDistances coords point = sum $ map (dist point) coords

calc :: [String] -> Int
calc ss =
  length [(x, y) | x <- [lef .. rit], y <- [bot .. top], allDistances points (x,y) < 10000]
  where
    points = map parseCoord ss
    (top, bot, lef, rit) = findBorders points

findBorders :: [Coord] -> (Int, Int, Int, Int)
findBorders c = (top, bottom, left, right)
  where
    horizontal = map fst c
    vertical = map snd c
    top = maximum vertical
    bottom = minimum vertical
    left = minimum horizontal
    right = maximum horizontal

main :: IO ()
main = do
  a <- loadDo calc "Day06_1.input"
  putStrLn $ show a
