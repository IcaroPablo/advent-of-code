import Control.Monad (liftM2, ap)
import Data.List (transpose)

lines2array :: [String] -> [[Int]]
lines2array = map (map read . words)

indexList :: [Int] -> [Int]
indexList = enumFromTo 0 . subtract 1 . length

generateCombination :: [Int] -> Int -> [Int]
generateCombination = flip (liftM2 (++) . take <*> drop . (1 +))

expandCombinations :: [Int] -> [[Int]]
expandCombinations = liftM2 map generateCombination indexList

isSequence :: [(Int, Int)] -> Bool
isSequence = liftM2 (||) (all (uncurry (<))) (all (uncurry (>)))

intervals :: [(Int, Int)] -> [Int]
intervals = map (abs . uncurry (-))

countValid :: [[[Int]]] -> Int
countValid = length
           . filter (any (all (<= 3)))
           . map (map intervals)
           . map (filter (isSequence))
           . map (map (zip <*> tail))

main :: IO()
main = readFile "sampled2.txt" >>= print . countValid
                                         . map (expandCombinations)
                                         . lines2array
                                         . lines
