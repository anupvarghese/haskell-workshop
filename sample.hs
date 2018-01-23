-- I am the normal function

sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

-- I am the guarded one

guardSumtorial n
  | n == 0 = 0
  | otherwise = n + guardSumtorial (n - 1)


-- List
emptyList = []

-- lets use cons

ex18 = 1 : []

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


-- find length of a List

listLength :: [Integer] -> Integer

listLength [] = 0
listLength (_:t) = 1 + listLength t

-- sum every 2 numbers in the listLength
sumEveryTwo :: [Integer] -> [Integer]

sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:z)) = (x + y) : sumEveryTwo z

-- Homework
{-
Credit card validation
Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled;
the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].
• Add the digits of the doubled values and the undoubled digits
from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18.
Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid
-}

toDigits :: Integer -> [Integer]
toDigits n
  | (n <= 0) = []
  | otherwise = (n `mod` 10) : (toDigits (n `div` 10))

reverseList :: [Integer] -> [Integer]

reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverseList (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:z)) = x : (2 * y) : (doubleEveryOther z)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (h:t) = (if h > 0 then (h `div` 10) + (h `mod` 10) else h) + sumDigits t

validate :: Integer -> Bool
validate number = if (sumDigits (doubleEveryOther (toDigits number)) `mod` 10 == 0) then True else False
