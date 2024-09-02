import qualified GHC.Float as Data.Float
-- !Mi resolución de la guía 4 de Haskell para Intro a la Programación.

-- * Ejercicio 1

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci(n-2)

-- * Ejercicio 2
