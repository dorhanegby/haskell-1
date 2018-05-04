-- Dor Hanegby
-- 204009633

-- 1.

isPalindrome :: String -> Bool
isPalindrome str = str == reverse(str)

-- 2.
isPrefix :: String -> String -> Bool
isPrefix (x:xs) [] = True
isPrefix [] (y:ys) = True
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys else False

-- 3.
-- a.

squareList :: Int -> [Int]
squareList 0 = [0]
squareList n = (n * n) : squareList (n - 1)

listSquare :: Int -> [Int]
listSquare 0 = [0]
listSquare n = listSquare (n - 1) ++ [(n * n)]

-- 4.

fact :: Int -> Int
fact n = fact' n 1

fact' :: Int -> Int -> Int
fact' 0 acc = acc
fact' n acc = fact' (n - 1) (acc * n)

-- 5.
-- a.

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = if n > 0 then toDigits (n `div` 10) ++ [(n `mod` 10)] else []

-- b.

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse (doubleEveryOther' (reverse (list)) False [])

doubleEveryOther' :: [Integer] -> Bool -> [Integer] -> [Integer]
doubleEveryOther' [] _ acc = acc
doubleEveryOther' (x:xs) isEvenIndex acc = if isEvenIndex then doubleEveryOther' xs (not isEvenIndex) (acc ++ [x * 2]) else doubleEveryOther' xs (not isEvenIndex) (acc ++ [x])

-- c.

sumDigitsOfNumber :: Integer -> Integer
sumDigitsOfNumber 0 = 0
sumDigitsOfNumber n = n `mod` 10 + sumDigitsOfNumber (n `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x: xs) = sumDigitsOfNumber x + sumDigits xs

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0





