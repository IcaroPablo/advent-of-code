import Control.Monad (liftM2)
import Text.Regex.PCRE
import Data.List (isInfixOf, isPrefixOf, transpose)

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

---------------------------------

repeatChar :: String -> Int -> String
repeatChar s n = concat (replicate n s)

-- shiftRight :: Int -> String -> String
shiftRight :: (Int, String) -> String
-- shiftRight x str = (repeatChar "." ((length str) - 1 - x)) ++ str ++ (repeatChar "." x)
shiftRight pair = (repeatChar "." ((length  (snd pair)) - 1 - (fst pair))) ++ (snd pair) ++ (repeatChar "." (fst pair))

shiftLeft :: (Int, String) -> String
-- shiftRight x str = (repeatChar "." ((length str) - 1 - x)) ++ str ++ (repeatChar "." x)
shiftLeft pair = (repeatChar "." (fst pair)) ++ (snd pair) ++ (repeatChar "." ((length  (snd pair)) - 1 - (fst pair)))

rotateRight :: [String] -> [String]
-- rotateRight ss = map (\pair -> shiftRight (fst pair) (snd pair)) (zip (enumFromTo 0 . subtract 1 . length ss) ss)
rotateRight ss = transpose (map (shiftRight) (zip (enumFromTo 0 ( subtract 1 ( length ss))) ss))
-- rotateRight ss = map (shiftRight) (zip ([0..]) ss)

rotateLeft :: [String] -> [String]
-- rotateRight ss = map (\pair -> shiftRight (fst pair) (snd pair)) (zip (enumFromTo 0 . subtract 1 . length ss) ss)
rotateLeft ss = transpose (map (shiftLeft) (zip (enumFromTo 0 ( subtract 1 ( length ss))) ss))
-- rotateRight ss = map (shiftRight) (zip ([0..]) ss)

countPattern :: String -> Int
countPattern str = length (extractexpr "XMAS" str)

concatenate :: [String] -> [String]
concatenate xs = xs
               ++ transpose xs
               ++ rotateRight xs
               ++ rotateLeft xs
               -- ++ rotate 45 pra lá xs
               -- ++ rotate 45 pra cá xs

main :: IO()
main = readFile "sampled4.txt" >>= print . sum
-- main = readFile "minisampled4.txt" >>= print
-- main = readFile "minisampled4.txt" >>= print . sum
-- main = readFile "minisampled4.txt" >>= putStrLn
-- main = do
--     let ls = readFile "minisampled4.txt"
--     putStrLn (lines ls)
                                         -- . map multiply
                                         -- . extractexpr "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
                                         -- . clean
                                         -- . head
                                         -- . unlines
                                         . map (\s -> (countPattern s) + (countPattern (reverse s)))
                                         -- . map (\s -> shiftRight 5 s)
                                         -- . rotateRight
                                         -- . rotateLeft
                                         . concatenate
                                         . lines
