module Defs where

-- fact :: Int -> Int
fact n
 | n == 0     = 1
 | n >  0     = n * fact (n - 1)
 | otherwise  = error "Подадено отрицателно число!"


square x = x * x

z = let x = 2
        y = 3
    in x + y

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

x □ 0 = -x - 2
x □ y = (x + 2) * (y - 1)


