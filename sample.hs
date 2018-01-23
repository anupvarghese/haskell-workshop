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
