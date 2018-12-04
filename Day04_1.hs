import Common
import Text.Parsec hiding (Line)
import Data.Either.Extra
import Data.List
import Data.Dates hiding (Time)
import qualified Data.Map.Strict as M

type Time = Int

data Event = NewShift Int | Sleep Time | Wake Time deriving (Show)
data Line = Line DateTime Event deriving (Show)

toEvent :: Line -> Event
toEvent (Line _ e) = e

instance Eq Line where
  (Line date1 _) == (Line date2 _) = date1 == date2

instance Ord Line where
  (Line date1 _) `compare` (Line date2 _) = date1 `compare` date2

parseLine :: String -> Line
parseLine = fromRight undefined . parse line ""
  where
    newShift = do
      string "Guard #"
      guardId <- read <$> many digit
      string " begins shift"
      return $ const $ NewShift guardId

    sleep = do
      string "falls asleep"
      return Sleep

    wake = do
      string "wakes up"
      return Wake

    line = do
      char '['
      year <- read <$> many digit
      char '-'
      month <- read <$> many digit
      char '-'
      day <- read <$> many digit
      spaces
      hour <- read <$> many digit
      char ':'
      minute <- read <$> many digit
      char ']'
      spaces
      event <- choice [newShift, sleep, wake]
      return $ Line (DateTime year month day hour minute 0) (event $ hour * 60 + minute)

data Guard = Guard Int [Time] deriving (Show)

addTimes :: Guard -> Int -> Int -> Guard
addTimes (Guard gid ts) st et = Guard gid $ ts ++ [st .. et-1]

toGuardList :: [Event] -> [Guard]
toGuardList = snd . foldl rule (Nothing, [])

rule :: (Maybe Int, [Guard]) -> Event -> (Maybe Int, [Guard])
rule (_, gs) (NewShift gId) = (Nothing, Guard gId [] : gs)
rule (_, gs) (Sleep t) = (Just t, gs)
rule (Just ts, g:gs) (Wake tw) = (Nothing, addTimes g ts tw : gs)

asTuple :: Guard -> (Int, [Time])
asTuple (Guard gId sleepTimes) = (gId, sleepTimes)

getSleeper :: [(Int, [Time])] -> (Int, [Time])
getSleeper = snd
             . maximumBy (\a b -> compare (fst a) (fst b))
             . map (\(gId, ts) -> (length ts, (gId, ts)))

getMostSleptMinute :: [Time] -> Time
getMostSleptMinute ts = fst
                       . maximumBy (\a b -> compare (snd a) (snd b))
                       . M.toList
                       . M.fromListWith (+)
                       $ zip ts (repeat 1)

getResult :: (Int, [Time]) -> Int
getResult (gId, ts) = gId * getMostSleptMinute ts

calc :: [String] -> Int
calc = getResult
       . getSleeper
       . M.toList
       . M.fromListWith (++)
       . map asTuple
       . toGuardList
       . map toEvent
       . sort
       . map parseLine

main :: IO ()
main = do
  bla <- loadDo calc "Day04_1.input"
  putStrLn . show $  bla
