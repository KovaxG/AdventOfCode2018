import Common
import Data.Either.Extra
import Data.List
import Text.Parsec

type Task = Char
data Rel = Before Task Task deriving (Show)

parseStep :: String -> Rel
parseStep = fromRight undefined . parse rule ""
  where
    rule = do
      string "Step"
      spaces
      first <- alphaNum
      spaces
      string "must be finished before step"
      spaces
      second <- alphaNum
      spaces
      string "can begin."
      return $ Before first second

go :: [Task] -> [Rel] -> [Task] -> [Task]
go done rules [] = done
go done [] rest = done ++ rest
go done rules rest = go (done ++ [doable]) (relax rules) (without rest)
  where
    doable = head $ filter (isDoable rules) rest

    relax :: [Rel] -> [Rel]
    relax = filter (\(Before t _) -> t /= doable)

    isDoable :: [Rel] -> Task -> Bool
    isDoable rules task = all (\(Before _ t) -> t /= task) rules

    without :: [Task] -> [Task]
    without = filter (/= doable)

calc :: [String] -> [Task]
calc ss = go [] rules tasks
  where
    rules = map parseStep ss
    tasks = distinct $ (\(Before a b) -> [a, b]) =<< rules

main :: IO ()
main = loadDoPrint calc "Day07_1.input"
