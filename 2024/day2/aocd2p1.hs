import Control.Monad (liftM2)

lines2array :: [String] -> [[Int]]
lines2array = map (map read . words)

isSequence :: [(Int, Int)] -> Bool
isSequence = liftM2 (||) (all (uncurry (<))) (all (uncurry (>)))

intervals :: [(Int, Int)] -> [Int]
intervals = map (abs . uncurry (-))

countValid :: [[Int]] -> Int
countValid = length
           . filter (all (<= 3))
           . map intervals
           . filter (isSequence)
           . map (zip <*> tail)

main :: IO()
main = readFile "sampled2.txt" >>= (print . countValid . lines2array . lines)
