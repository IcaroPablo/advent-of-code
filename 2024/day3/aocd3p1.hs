import Control.Monad (liftM2)
import Text.Regex.Posix

extractexpr :: String -> String -> [String]
extractexpr = (getAllTextMatches .) . flip (=~)

multiply :: String -> Int
multiply = liftM2 (*) (head) (last) . map read . extractexpr "[0-9]{1,3}"

main :: IO()
main = readFile "sampled3.txt" >>= print . sum
                                         . map multiply
                                         . extractexpr "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
                                         . head
                                         . lines
