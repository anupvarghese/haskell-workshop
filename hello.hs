x = 3

-- Normal function
f :: Integer -> Integer
f i = i + 10

--  Lambda Expression
ff :: Integer -> Integer
ff = \i -> i + 10


-- Another lambda
fff :: Integer -> Integer -> Integer
fff = \a b -> (a +b) * 2

-- Another lambda

ffff :: (Integer -> Integer) -> Integer
ffff k = k 99

-- *Main> let a = fff 10
-- *Main> a
-- *Main> (fff 55) 77
-- 264

-- *Main> fff 20 50
-- 140
-- *Main> 20 fff 50

-- *Main> 20 `fff` 50
-- 140

-- *Main> (+) 20 50
-- 70
-- *Main> 20 + 50
-- 70

-- type is anything to anything
-- a -> a
-- *Main> :t g
-- g :: t -> t
g s = s

-- Data types
pie = 3
data Shape = Circle Integer | Rectangle Integer Integer | Triangle Integer Integer Integer
  deriving Show

instance Eq Shape where
  -- Shape -> Shape -> bool
  (==) =
    \s1 s2 ->
      case s1 of
        Circle r1 ->
          case s2 of
            Circle r2 ->
              r1 == r2
            _ ->
              False
        Rectangle w1 h1 ->
          case s2 of
            Rectangle w2 h2 ->
              (w1 == w2 ) && (h1 == h2)
            _ ->
                False
        Triangle a1 b1 c1 ->
          case s2 of
            Triangle a2 b2 c2 ->
              (a1 == a2) && (b1 ==b2) && (c1 == c2)
            _ ->
              False


  -- What if Eq type class does not exist

class Eek a where
  (===) :: a -> a -> Bool

instance Eek Shape where
  (===) =
    \s1 s2 -> False

-- if not defined
-- *Main> Circle 99 === Circle 99

-- <interactive>:78:11:
--     No instance for (Eek Shape) arising from a use of `==='
--     In the expression: Circle 99 === Circle 99
--     In an equation for `it': it = Circle 99 === Circle 99
-- *Main>

-- A function to compute perimeter of shape

perimeter :: Shape -> Integer
-- Another form of pattern matching
perimeter (Circle r) = r * 2 * pie
perimeter (Rectangle w h) = (w + h) * 2
perimeter (Triangle a b c) = a + b + c

-- *Main> perimeter (Circle 9)
-- 54
-- *Main> perimeter (Triangle 9 10 20)
-- 39
