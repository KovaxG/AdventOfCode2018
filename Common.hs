module Common where

loadDo :: ([String] -> a) -> String -> IO a
loadDo calc path = calc . lines <$> readFile path


count :: Eq a => a -> [a] -> Int
count n = sum . map (\a -> if a == n then 1 else 0)


sliding :: Int -> ([a] -> b) -> [a] -> [b]
sliding nr f as = reverse $ go nr f as []
  where
    go nr f as bs 
      | length as >= nr = 
        let nbs = f (take nr as) : bs
            nas = tail as
        in go nr f nas nbs
      | otherwise = bs

{-
  histo :: Ord a => [a] -> Map.Map a Int
  histo = foldl (\hs a -> Map.insertWith (+) a 1 hs) Map.empty
-}
