-- You
slow_fib :: (Integral a) => a -> a
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n - 1) + slow_fib (n - 2)

-- The guy she told you not to worry about
matrix_mult :: (Integral a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
matrix_mult (a,b,c,d) (e,f,g,h) = 
    ((a*e + b*g), (a*f + b*h), (c*e + d*g), (c*f + d*h))

-- mk_expon :: (Integral a) => (t -> t -> t) -> t -> (t -> a -> t)
-- mk_expon times one =
--     let expon x n =
--             if n == 0 then one
--             else if (n `mod` 2) == 0 then expon (times x x) (div n 2)
--             else times x (expon x (n-1))
--     in expon

mk_expon :: (Integral a) => (t -> t -> t) -> t -> (t -> a -> t)
mk_expon times one = expon
    where expon x n
            | n == 0 = one
            | n `mod` 2 == 0 = expon (times x x) (div n 2)
            | otherwise = times x (expon x (n-1))

mat_exp = mk_expon matrix_mult identity
    where identity = (1,0,0,1)

fast_af_fib :: Integer -> Integer
fast_af_fib n = c
    where (a,b,c,d) = mat_exp (1,1,1,0) n

-- Others 
reg_exp = mk_expon (*) 1
string_exp = mk_expon (++) ""


--- 

collect :: (a -> [b]) -> [a] -> [b]
collect _ [] = []
collect f (x:xs) = (f x) ++ (collect f xs)