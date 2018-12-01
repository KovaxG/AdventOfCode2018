calc :: [String] -> Int
calc = go 0 [0] . concat . repeat . map read . map (filter (/= '+'))
  where
    go :: Int -> [Int] -> [Int] -> Int
    go current old (change:rest) =
      let new = current + change
      in if elem new old
         then new
         else go new (new : old) rest

main :: IO ()
main = do
  putStrLn "I strongly advise that you compile this code first"
  putStrLn "since even so it takes like 5 minutes to compute."
  content <- lines <$> readFile "Day01_1.input"
  putStrLn "You have been warned. Starting computation ..."
  let twice = calc content
  putStrLn $ show twice
  putStrLn "Finally, we have the answer!"
  end <- getLine
  return ()
