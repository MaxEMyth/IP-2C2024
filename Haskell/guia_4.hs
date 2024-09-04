-- !Mi resolución de la guía 4 de Haskell para Intro a la Programación.

-- * Ejercicio 1

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci(n-2)

-- * Ejercicio 2
parteEntera :: Float -> Integer
{-
  problema parteEntera (x: R) : Z {
    requiere: {True }
    asegura: { resultado ≤ x < resultado + 1 }
    }
-}
{- Solución con Comprensión
parteEntera x = head [i | i <- l x, (fromInteger i)<=x, x<(fromInteger (i+1))]
 where 
  l x
   | x>=0 = [0, 1..]
   | x < 0 = [0, -1..]
-}