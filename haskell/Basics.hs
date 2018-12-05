module Basics where

fact 0 = 1
fact n = n * fact (n - 1)

x :: Integer
x = 42
y :: Integer
y = fact 20000

z = fromIntegral x^2 + 2.4

t :: Integer
t = 15

myfun x y = 2 * x - y

test = if (2 == 2) then fact 20000 else 15
