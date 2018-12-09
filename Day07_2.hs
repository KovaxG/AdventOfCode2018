import Common
import Data.Char
import Data.Either.Extra
import Data.List
import Text.Parsec
import Debug.Trace

data Task = Task Int Char deriving (Show, Eq)
data Rel = Before Char Char deriving (Show)

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

go :: Int -> [Task] -> [Rel] -> [Task] -> Int
go ts [] _ [] = ts
go ts rest rules working =
  go (ts + 1) (removeDoneAndWorking done working' rest) (rules') working'
  where
    (done, working', available', rules') = update working

    update :: [Task] -> ([Task], [Task], [Task], [Rel])
    update working =
      let done = filter (\(Task i _) -> i == 0) w
          newRules = relax done rules
          workingOn = filter (\(Task i _) -> i /= 0) w
          available = filter (isDoable newRules) rest
          nr = 5
          newTasks = take (nr - length workingOn) available
          aval = drop (nr - length workingOn) available
      in (done, workingOn ++ newTasks, aval, newRules)
      where
        w = map (\(Task t c) -> Task (t-1) c) working

    removeDoneAndWorking :: [Task] -> [Task] -> [Task] -> [Task]
    removeDoneAndWorking done ws =
      filter (\(Task _ c) -> not $ elem c workingChars)
      . filter (\(Task _ c) -> not $ elem c doneChars)
      where
        doneChars = map (\(Task _ c) -> c) done
        workingChars = map (\(Task _ c) -> c) ws

    relax :: [Task] -> [Rel] -> [Rel]
    relax done = filter (\(Before t _) -> not $ elem t ts)
      where ts = map (\(Task _ c) -> c) done

    isDoable :: [Rel] -> Task -> Bool
    isDoable rules task = all (\(Before _ t) -> t /= c) rules
      where Task _ c = task

calc :: [String] -> Int
calc ss = go 0 tasks rules [] - 1
  where
    rules = map parseStep ss
    tasks = map (\c -> Task (ord c - 4) c) $ distinct $ (\(Before a b) -> [a, b]) =<< rules

main :: IO ()
main = loadDoPrint calc "Day07_1.input"
