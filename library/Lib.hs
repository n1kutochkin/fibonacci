module Lib where

-- "Наивный" рекурсивный вариант
fibN :: Integer -> Integer
fibN 0 = 1
fibN 1 = 1
fibN n = fibN (n-1) + fibN (n-2)

-- Вариант, использующий хвостовую рекурсию
fibNTailRecursive :: Integer -> Integer
fibNTailRecursive x = _fib x 1 1

_fib :: Integer -> Integer -> Integer -> Integer
_fib n ppre pre | n <= 1 = ppre
_fib n ppre pre = _fib (n-1) pre (ppre + pre)

-- "Наивная" реализация на основе fibNTailRecursive и map
fibNaive :: [Integer]
fibNaive = map fibNTailRecursive [1..]

-- Рекурсивное определение с использованием zipWith
fib :: [Integer]
fib = 0 : _next
    where _next = 1 : zipWith (+) fib _next
