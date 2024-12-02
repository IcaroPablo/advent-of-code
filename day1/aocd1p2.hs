import Data.List (sort, transpose, nub, group)

lines2array :: [String] -> [[Int]]
lines2array xs = transpose $ map (map read . words) xs

frequencyMap :: [Int] -> Int -> Int
frequencyMap list n = 
    let frequencies = map length $ group $ sort list
        uniqueNumbers = nub $ sort list
    in case lookup n (zip uniqueNumbers frequencies) of
        Just count -> count * n
        Nothing    -> 0

cal :: [[Int]] -> Int
cal x = sum $ map (frequencyMap (head x)) (last x)

main :: IO()
main = readFile "sample.txt" >>= (print . cal . lines2array . lines)
