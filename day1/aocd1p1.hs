import Data.List (sort, transpose)

lines2array :: [String] -> [[Int]]
lines2array xs = map (map read . words) xs

diffs :: [[Int]] -> [Int]
diffs xs = map (abs . uncurry (-)) (zip (head xs) (last xs))

sumDiffs :: [String] -> Int
sumDiffs xs = sum . diffs $ map sort (transpose (lines2array xs))

main :: IO()
main = readFile "sample.txt" >>= (print . sumDiffs . lines)
