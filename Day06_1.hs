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

data Mark = Dot | Point Coord deriving (Show, Ord, Eq)

markCoord :: [Coord] -> Coord -> Mark
markCoord coords point = if nr > 1 then Dot else Point (snd closestDist)
  where
    distsToCoord = zip (map (dist point) coords) coords
    closestDist = minimumBy (\a b -> fst a `compare` fst b) distsToCoord
    nr = countThat (\(d, _) -> d == fst closestDist) distsToCoord

getBorderValues :: Matrix.Matrix Mark -> [Mark]
getBorderValues m = distinct
                    $ Vector.toList (Matrix.getCol 1 m)
                    ++ Vector.toList (Matrix.getCol lastCol m)
                    ++ Vector.toList (Matrix.getRow 1 m)
                    ++ Vector.toList (Matrix.getRow lastRow m)
  where
    lastCol = Matrix.ncols m
    lastRow = Matrix.nrows m

--calc :: [String] -> [Coord]
calc ss = snd
          . maximumBy (\a b -> snd a `compare` snd b)
          . filter (\(p, _) -> not $ elem p borderValues)
          . Map.toList
          . Map.fromListWith (+)
          . map (\m -> (m, 1))
          $ Matrix.toList marked
  where
    borderValues = Dot : getBorderValues marked
    marked = Matrix.matrix (top - bot) (rit - lef) $ \(x, y) -> markCoord points (x + lef, y + bot)
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
