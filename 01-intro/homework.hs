-- Exercise 1 We need to first find the digits of a number

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (reverse . toDigitsRev) (n)

-- Exercise 2 Once we have the digits in the proper order,
-- we need to double every other one.

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : (y * 2) : doubleEveryOther zs

-- Exercise 3 The output of doubleEveryOther has a mix of one-digit and
-- two-digit numbers. Define the function
--    sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:xs) = sumDigits(toDigitsRev(x)) + sumDigits(xs)

-- Exercise 4 Define the function
--  validate :: Integer -> Bool
-- that indicates whether an integer could be a valid credit card number.
-- This will use all functions defined in previous exercises

validate :: Integer -> Bool
validate n
  | n < 0 = False
  | otherwise = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0
