import Text.Regex.PCRE
import Data.List (transpose)

extractexpr :: String -> String -> [String]
extractexpr = (getAllTextMatches .) . flip (=~)

getSubmatrix :: Int -> Int -> [[a]] -> [[a]]
getSubmatrix r c mat = take 3 $ map (take 3 . drop c) (drop r mat)

extract3x3 :: [[a]] -> [[[a]]]
extract3x3 mat = [getSubmatrix i j mat | i <- [0..rows-3], j <- [0..cols-3]]
    where
        rows = length mat
        cols = length (head mat)

repeatChar :: String -> Int -> String
repeatChar s n = concat (replicate n s)

shiftRight :: (Int, String) -> String
shiftRight pair = (repeatChar "." ((length  (snd pair)) - 1 - (fst pair))) ++ (snd pair) ++ (repeatChar "." (fst pair))

shiftLeft :: (Int, String) -> String
shiftLeft pair = (repeatChar "." (fst pair)) ++ (snd pair) ++ (repeatChar "." ((length  (snd pair)) - 1 - (fst pair)))

rotateRight :: [String] -> [String]
rotateRight ss = transpose (map (shiftRight) (zip (enumFromTo 0 ( subtract 1 ( length ss))) ss))

rotateLeft :: [String] -> [String]
rotateLeft ss = transpose (map (shiftLeft) (zip (enumFromTo 0 ( subtract 1 ( length ss))) ss))

countPattern :: String -> Int
countPattern str = length ((extractexpr "MAS" str) ++ (extractexpr "SAM" str))

concatenate :: [String] -> [String]
concatenate xs = rotateRight xs ++ rotateLeft xs

countValid :: [String] -> Int
countValid = length
           . filter ((==) 4)
           . map (sum . map (\s -> (countPattern s) + (countPattern (reverse s))) . concatenate)
           . extract3x3

main :: IO()
main = readFile "sampled4.txt" >>= print . countValid . lines
