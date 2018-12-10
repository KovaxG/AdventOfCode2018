data Tree = Node [Tree] [Int] deriving (Show)

parseTree :: [Int] -> Tree
parseTree = fst . parseNode
  where
    parseNode :: [Int] -> (Tree, [Int])
    parseNode (chnr:mdnr:rest) =
      let (children, remaining) = foldl parseChildren ([], rest) [1..chnr]
          (metaData, leftOver) = splitAt mdnr remaining
      in (Node children metaData, leftOver)

    parseChildren :: ([Tree], [Int]) -> Int -> ([Tree], [Int])
    parseChildren (ts, n) _ =
      let (t, nn) = parseNode n
      in (ts ++ [t], nn)

metaDataSum :: Tree -> Int
metaDataSum (Node [] metadata) = sum metadata
metaDataSum (Node children indexes) =
  sum $ map (\i -> maybe 0 metaDataSum $ safeGet i children) indexes

calc :: [String] -> Int
calc = metaDataSum . parseTree . map read

main :: IO ()
main = do
  cont <- calc . words <$> readFile "Day08_1.input"
  putStrLn . show $ cont

safeGet :: Int -> [a] -> Maybe a
safeGet i as
  | length as >= i && i > 0 = Just (as !! (i-1))
  | otherwise = Nothing
