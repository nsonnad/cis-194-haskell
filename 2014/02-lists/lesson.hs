import Data.List

-- local variables

-- using `let`
-- don't forget to use `in` after defining variable
strLength :: String -> Int
strLength [] = 0
strLength (_:xs) = let len_rest = strLength(xs) in
                   len_rest + 1

-- using `where` clauses
frob :: String -> Char
frob [] = 'a'
frob str
  | len > 5   = 'x'
  | len < 3   = 'y'
  | otherwise = 'z'
  where
    len = strLength str

-- the only way to repeat a computation is with recursion
-- so how do we write something that counts numbers until getting to 20?
-- use an accumulator

sumTo20 :: [Int] -> Int
sumTo20 nums = sumTo20Helper 0 nums  -- accumulator starts at 0

sumTo20Helper :: Int -> [Int] -> Int
sumTo20Helper acc [] = acc
sumTo20Helper acc (x:xs)
  | acc >= 20 = acc
  | otherwise = sumTo20Helper (acc + x) xs
