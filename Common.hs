module Common where

loadDo :: ([String] -> a) -> String -> IO a
loadDo calc path = calc . lines <$> readFile path 


count :: Eq a => a -> [a] -> Int
count n = sum . map (\a -> if a == n then 1 else 0)
