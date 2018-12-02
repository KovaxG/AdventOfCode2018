import Common

calc :: [String] -> String
calc ss = getAns . filter ((==1) . fst) $ map distance combinations
  where
    combinations = traverse (\_ -> ss) $ replicate 2 ()
    getAns ((_, (s1:s2:_)):_) = map fst . filter (\(a,b) -> a == b) $ zip s1 s2

distance :: [String] -> (Int, [String])
distance s@(s1:s2:_) = (\a -> (a, s)) . sum . map countEq $ zip s1 s2
  where
    countEq (a, b) = if a /= b then 1 else 0

main :: IO ()
main = loadDo calc "Day02_1.input" >>= putStrLn
