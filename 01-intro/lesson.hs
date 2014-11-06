sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = []
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
