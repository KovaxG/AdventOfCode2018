import Common
import Data.Char

type Polimer = String

react :: Polimer -> Polimer
react = reverse . foldl rule ""
  where
    rule :: Polimer -> Char -> Polimer
    rule [] c = [c]
    rule (h:t) c
      | differentCase c h && sameCharacter c h = t
      | otherwise = (c:h:t)      
  
    differentCase a b = 
      isUpper a && isLower b || isLower a && isUpper b
      
    sameCharacter a b = toUpper a == toUpper b      

calc :: [String] -> Int
calc = length . react . unlines

main :: IO ()
main = do
  len <- loadDo calc "Day05_1.input"
  putStrLn $ show len