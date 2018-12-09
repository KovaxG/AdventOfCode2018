module Common where

import qualified Data.Matrix as Matrix
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

loadDo :: ([String] -> a) -> String -> IO a
loadDo calc path = calc . lines <$> readFile path

loadDoPrint :: Show a => ([String] -> a) -> String -> IO ()
loadDoPrint calc path = show . calc . lines <$> readFile path >>= putStrLn

count :: Eq a => a -> [a] -> Int
count n = length . filter (==n)

countThat :: (a -> Bool) -> [a] -> Int
countThat f = length . filter f

sliding :: Int -> ([a] -> b) -> [a] -> [b]
sliding nr f as = reverse $ go nr f as []
  where
    go nr f as bs
      | length as >= nr =
        let nbs = f (take nr as) : bs
            nas = tail as
        in go nr f nas nbs
      | otherwise = bs

distinct :: Ord a => [a] -> [a]
distinct = map fst
           . Map.toList
           . Map.fromList
           . map (\a -> (,) a ())

{-
  histo :: Ord a => [a] -> Map.Map a Int
  histo = foldl (\hs a -> Map.insertWith (+) a 1 hs) Map.empty
-}
