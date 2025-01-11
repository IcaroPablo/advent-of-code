import Text.Regex.PCRE
import Data.List (isInfixOf, transpose)
import Data.List.Split (splitOn)
-- import Data.Text (splitOn)

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

isRule :: String -> Bool
isRule = isInfixOf "|"

-- buildRules :: [String] -> ([(Int, Int)], [[Int]])
-- buildRules :: [String] -> [(Int, Int)]
-- buildRules :: [String] -> [Int]
-- buildRules :: [String] -> [String]
buildRules :: [String] -> [[String]]
-- buildRules xs = map splitOn (filter (isRule) xs)
buildRules xs = map (\s -> splitOn "|" s) (filter (isRule) xs)

buildLists :: [String] -> [[String]]
buildLists xs = map (splitOn ",") (filter (isInfixOf ",") xs)

buildPairs :: [String] -> [[String]]
buildPairs xs = if (length xs) > 1
                    then (map (\i -> [head xs, i]) (tail xs)) ++ (buildPairs (tail xs))
                    else []

isValidPair :: [String] -> [String] -> Bool
isValidPair pair xs = pair `elem` (buildRules xs)

-- filterValid :: [[[String]]] -> [[[String]]]
-- filterValid xs = filter (\pairList -> all (\pair -> pair `elem` (buildRules xs)) pairList) xs
-- filterValid :: [String] -> [String]
-- filterValid :: [String] -> [[[String]]]
-- filterValid xs = let pairListList = map (buildPairs) (buildLists xs)
--                  -- in filter (\pairList -> all (\pair -> pair `elem` (buildRules xs)) pairList) pairListList
--                  in filter (\pairList -> all (\pair -> isValidPair pair xs) pairList) pairListList

getMedian :: [String] -> Int
getMedian xs = read (xs !! ((length xs) `div` 2))

filterValid :: [String] -> [[String]]
filterValid xs = filter (\itemList -> all (\pair -> isValidPair pair xs) (buildPairs itemList)) (buildLists xs)
                 -- in filter (\pairList -> all (\pair -> pair `elem` (buildRules xs)) pairList) pairListList
                 -- in filter (\pairList -> all (\pair -> isValidPair pair xs) pairList) pairListList

-- gerar a lista de listas
-- para cada lista de listas, gerar os pares
-- filtrar as que tem pares válidos

main :: IO()
main = readFile "sampled5.txt" >>= print
-- main = readFile "minisampled5.txt" >>= putStrLn
-- main = readFile "minisampled5.txt" >>= print
                                 -- . countValid
                                 -- . unlines
                                 . sum
                                 . map (getMedian)
                                 . filterValid
                                 -- . map (buildPairs)
                                 -- . buildLists
                                 . lines

-- dada uma lista, eu preciso gerar os pares
-- dados os pares eu preciso buscar as regras e verificar se o par é válido
-- se todos os pares forem válidos a lista é válida
-- buscar o número do meio
-- somar
