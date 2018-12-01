calc :: [String] -> Int
calc = sum . map read . map (filter (/='+'))

main :: IO ()
main = do
  content <- lines <$> readFile "Day01_1.input"
  let freq = calc content
  putStrLn $ show freq
