import Data.List (sort, transpose)

diffs :: [[Int]] -> [Int]
diffs xs = map (abs . uncurry (-)) (zip (head xs) (last xs))

main :: IO()
main = do
    content <- readFile "sample.txt"
    let inputList = lines content
    let splitted = map (map read . words) inputList
    let sorted = map sort (transpose splitted)
    
    print (sum (diffs sorted))
