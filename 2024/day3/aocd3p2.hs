import Control.Monad (liftM2)
import Text.Regex.PCRE
import Data.List (isInfixOf, isPrefixOf)

extractexpr :: String -> String -> [String]
extractexpr = (getAllTextMatches .) . flip (=~)

removeFirst :: String -> String -> String
removeFirst _ [] = []
removeFirst target str
  | target `isPrefixOf` str = drop (length target) str
  | otherwise = case str of
      [] -> []
      (x:xs) -> x : removeFirst target xs

delPCRE :: String -> String -> String
delPCRE regex str = removeFirst (str =~ regex :: String) str

multiply :: String -> Int
multiply = liftM2 (*) (head) (last) . map read . extractexpr "[0-9]{1,3}"

clean :: String -> String
clean str = if isInfixOf "don't()" str
                then (if length (extractexpr "don\'t\\(\\).*do\\(\\)" str) > 0
                          then (clean (delPCRE "don\'t\\(\\)(.*?)(?=do\\(\\))" str))
                          else (clean (delPCRE "don\'t\\(\\).*" str)))
                else str

main :: IO()
main = readFile "sampled3.txt" >>= print . sum
                                         . map multiply
                                         . extractexpr "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
                                         . clean
                                         . head
                                         . lines
