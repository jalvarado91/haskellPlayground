{-- 
    Ex1. Validating Credit Card Numbers
    1. Double the value of every second digit beginning
       from the right. [1,3,8,6] -> [2,3,16,6]
    2. Add the digits of the doubled values and undoubled digits
       from the original number. [2,3,16,6] -> 2+3+1+6+6 = 18
    3. Calculate the remainder when the sum is divided by 10. 
       18 % 10 = 8 
    4. If result == 0, then the number is valid.
--}

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (div n 10) ++ [(mod n 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (mod n 10) : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) 
    | (length (x:y:zs)) `mod` 2 == 0  = x*2:y:doubleEveryOther zs
    | otherwise                       = x:y*2:doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n =
    sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0