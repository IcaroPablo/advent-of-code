import Data.List (sort, transpose)

diffs :: [[Int]] -> [Int]
diffs sArray =
    map (abs . uncurry (-)) (zip (head sArray) (last sArray))

main :: IO()
main = do
    content <- readFile "sample.txt"
    let inputList = lines content
    let splitted = map (map read . words) inputList
    let sorted = map sort (transpose result)
    
    print (sum (diffs sorted))
