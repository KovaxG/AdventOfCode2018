import Common

calc :: [String] -> Int
calc = mulTuple . foldl addTuples (0,0) . map twosAndThrees
  where
    addTuples (a,b) (c,d) = (a + c, b + d)
    mulTuple (a, b) = a * b

twosAndThrees :: String -> (Int, Int)
twosAndThrees s = (twos, threes)
  where
    nrs = counts s
    twos = if count 2 nrs > 1 then 1 else 0
    threes = if count 3 nrs > 1 then 1 else 0 

counts :: String -> [Int]
counts s = map (\a -> count a s) s 

main :: IO ()
main = do
  checksum <- loadDo calc "Day02_1.input"
  putStrLn $ show checksum

test = [
 "abcdef", 
 "bababc", 
 "abbcde", 
 "abcccd", 
 "aabcdd", 
 "abcdee", 
 "ababab" 
  ]
