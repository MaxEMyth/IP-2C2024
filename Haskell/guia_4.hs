-- !Mi resolución de la guía 4 de Haskell para Intro a la Programación.

-- * Ejercicio 1

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n -1) + fibonacci (n -2)

-- * Ejercicio 2

parteEntera :: Float -> Integer
{-
  problema parteEntera (x: R) : Z {
    requiere: {True }
    asegura: { resultado ≤ x < resultado + 1 }
    }
-}
-- Solución con Comprensión
{-
  parteEntera x = head [i | i <- l x, (fromInteger i)<=x, x<(fromInteger (i+1))]
  where
    l x
    | x>=0 = [0, 1..]
    | x < 0 = [0, -1..]
-}
parteEntera x
  | x >= 1 = parteEntera (x -1)
  | x < 0 = parteEntera (x + 1)
  | otherwise = 0

-- * Ejercicio 3

esDivisible :: Integer -> Integer -> Bool
{-
  problema esDivisible (n:Z, d:Z) {
    Requiere: {n>0, d>0}
    Asegura: {res = True <=> Existe k en Z tq n = k*d}
  }
-}
esDivisible n d
  | n == 0 = True
  | n < 0 = False
  | otherwise = esDivisible (n - d) d


