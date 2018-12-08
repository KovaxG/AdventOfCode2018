import Common
import Data.Either.Extra
import Text.Parsec

type Coord = (Int, Int)

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

--calc :: [String] -> [Coord]
calc ss = borders
  where 
    points = map parseCoord ss
    borders = findBorders        points
  
findBorders :: [Coord] -> (Int, Int, Int, Int)
findBorders c = (top, bottom, left, right)
  where
    top = 1
    bottom = 1
    left = 1
    right = 1

main :: IO ()
main = do
  a <- loadDo calc "Day06_1.input"
  putStrLn $ show a