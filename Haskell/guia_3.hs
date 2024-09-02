-- !Mi resolución de la guía 3 de Haskell para Intro a la Programación.

-- * Ej 1.a

ej_1a_f :: Integer -> Integer
ej_1a_f 1 = 8
ej_1a_f 4 = 131
ej_1a_f 16 = 16

-- * Ej 1.b

ej_1b_g :: Integer -> Integer
ej_1b_g 8 = 16
ej_1b_g 16 = 4
ej_1b_g 131 = 1

-- * Ej 1.c

ej_1c_h :: Integer -> Integer
ej_1c_h = ej_1a_f . ej_1b_g

ej_1c_k :: Integer -> Integer
ej_1c_k = ej_1b_g . ej_1a_f

-- * Ej 2.a

ej_2a_absoluto :: Integer -> Integer
{-
  problema absoluto(x: Z): Z {
    Requiere: {True}
    Asegura: {res = x <=> x>=0}
    Asegura: {res = x <=> x < 0}
  }
-}
-- Alternativamente; ej_2a_absoluto = abs
ej_2a_absoluto x
  | x >= 0 = x
  | otherwise = -x

-- * Ej 2.b

ej_2b_maximoAbsoluto :: Integer -> Integer -> Integer
{-
  problema maximoAbsoluto(x:Z, y:Z): Z {
    Requiere: {True}
    Asegura: {res = absoluto(x) <=> absoluto(x)>=absoluto(y)}
    Asegura: {res = absoluto(y) <=> absoluto(x) < absoluto(y)}
  }
-}
ej_2b_maximoAbsoluto x y
  | ej_2a_absoluto x >= ej_2a_absoluto y = x
  | otherwise = y

-- * Ej 2.c

ej_2c_maximo3 :: Integer -> Integer -> Integer -> Integer
{-
  problema maximo3(a:Z, b:Z, c:Z):Z {
    Requiere: {True}
    Asegura: {res = a <=> a>=b && a>=c)}
    Asegura: {res = b <=> b>=a && b>=c)}
    Asegura: {res = c <=> c>=a && c>=b)}
  }
-}
ej_2c_maximo3 a b c
  | a >= b && a >= c = a
  | b >= a && b >= c = b
  | c >= a && c >= b = c

-- * Ej 2.d

ej_2d_algunoEs0 :: Float -> Float -> Bool
{-
  problema algunoEs0(x:R, y:R):Bool {
    Requiere: {True}
    Asegura: {res = True <=> (x==0||y==0)}
  }
-}
{-
Usando Pattern Matching:
 ej_2d_algunoEs0 0 _ = True
 ej_2d_algunoEs0 _ 0 = True
 ej_2d_algunoEs0 _ _ = False

Usando Guardas:
 ej_2d_algunoEs0 x y
  | x==0||y==0 = True
  |otherwise = False
-}
ej_2d_algunoEs0 x y = x == 0 || y == 0

-- * Ej 2.e

ej_2e_ambosSon0 :: Float -> Float -> Bool
{-
  problema ambosSon0(x:R, y:R):Bool {
    Requiere: {True}
    Asegura: {res = True <=>(x==0&&y==0)}
  }
-}
{-
Usando Pattern Matching:
ej_2e_ambosSon0 0 0 = True
ej_2e_ambosSon0 _ _ = False

Usando Guardas:
ej_2e_ambosSon0 x y
 |x==0 && y y==0
 |otherwise = False
-}
ej_2e_ambosSon0 x y = x == 0 && y == 0

-- * Ej 2.f

ej_2f_mismoIntervalo :: Float -> Float -> Bool
{-
  problema mismoIntervalo(x:R, y:R): Bool {
    Requiere: {True}
    Asegura: {res = True <=> (x<=3&&y<=3)||(3<x&&x<=7&&3<y<=7)||(7<x&&7<y)}
  }
-}
ej_2f_mismoIntervalo x y = x <= 3 && y <= 3 || 3 < x && x <= 7 && 3 < y && y <= 7 || 7 < x && 7 < y

-- * Ej 2.g

ej_2g_sumaDistintos :: Float -> Float -> Float -> Float
{-
  problema sumaDistintos(a: Z, b: Z, c: Z): Z {
    Requiere: {True}
    Asegura: {res = a + b + c <=> a != b && a!=c && b!=c}
    Asegura: {res = a + b <=> (a==c||b==c)&&(a!=b)} --!Creo que estos aseguras están mal.
    Asegura: {res = a + c <=> (a==b||c==b)&&(a!=c)}
    Asegura: {res = b + c <=> (b==a||c==a)&&(b!=c)}
    Asegura: {res = a <=> (a==b==c)}
  }
-}
ej_2g_sumaDistintos a b c
  | a == b && b == c = a -- Caso todos iguales
  | a /= b && b /= c && a /= c = a + b + c -- Caso todos diferentes
  | a == b || b == c = a + c -- Si alguno de estos son iguales
  | otherwise = a + b -- Último caso. (a == c)

-- * Ej 2.h

ej_2h_esMultiploDe :: Integer -> Integer -> Bool
{-
  problema esMultiploDe(n:Z, m:Z ): Bool {
    Requiere: {n>0}
    Requiere: {m>0}
    Asegura: {res = True <=> n es múltiplo de m}
  }
-}
ej_2h_esMultiploDe n m = mod n m == 0

-- * Ej 2.i

ej_2i_digitoUnidades :: Integer -> Integer
{-
  problema digitoUnidades(n:Z): Z {
    Requiere: {True}
    Asegura: {res = n mod 10}
  }
-}
ej_2i_digitoUnidades n = mod (abs n) 10

-- * Ej 2.j

ej_2j_digitoDecenas :: Integer -> Integer
{-
  problema digitoDecenas(n: Z): Z {
    Requiere: {n>9}
    Asegura: {res = (n//10) mod 10}
  }
-}
ej_2j_digitoDecenas n
  | n > 9 = mod (div n 10) 10
  | otherwise = undefined

-- * Ej 3

ej_3_estanRelacionados :: Integer -> Integer -> Bool
{-
  problema estanRelacionados (a:Z, b:Z) : Bool {
    Requiere: {a ̸= 0 ∧ b ̸= 0}
    Asegura: {res = true <=> a ∗ a + a ∗ b ∗ k = 0 para algún k ∈ Z con k!=0)}
  }
-}
-- ? Observo: a² + abk = 0 <=> a + bk = 0 (pues a/=0)
-- ? Entonces: res = True <=> a = b * (-k) <=> a es múltiplo de b
ej_3_estanRelacionados a b
  | a /= 0 && b /= 0 = mod a b == 0
  | otherwise = undefined

-- * Ej 4.a

ej_4a_prodInt :: (Float, Float) -> (Float, Float) -> Float
{-
  problema productoInterno(a:(RxR), b:(RxR)): R {
    Requiere: {True}
    Asegura: {res = fst(a)*fst(b) + snd(a)*snd(b)}
  }
-}
ej_4a_prodInt (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- * Ej 4.b

ej_4b_todoMenor :: (Float, Float) -> (Float, Float) -> Bool
{-
  problema todoMenor(a:(RxR), b:(RxR)): Bool {
    Requiere: {True}
    Asegura: {res = True <=> fst(a)<fst(b)&&snd(a)<snd(b)}
  }
-}
ej_4b_todoMenor (x1, y1) (x2, y2) =
  x1 < x2 && y1 < y2

-- * Ej 4.c

ej_4c_distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
{-
  problema distanciaPuntos(a:(RxR), b:(RxR)): R {
    Requiere: {True}
    Asegura: {res = sqrt((fst(a)-fst(b))^2 + (snd(a)-snd(b))^2)}
  }
-}
ej_4c_distanciaPuntos (x1, y1) (x2, y2) =
  sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- * Ej 4.d

ej_4d_sumaTerna :: (Integer, Integer, Integer) -> Integer
{-
  problema sumaTerna(t: (ZxZxZ)): Z {
    Requiere: {True}
    Asegura: {res = t_1 + t_2 + t_3}
  }
-}
ej_4d_sumaTerna (x, y, z) = x + y + z

-- * Ej 4.e

ej_4e_sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
{-
  problema sumarSoloMultiplos(t: (ZxZxZ), n:Z): Z {
    Requiere: {n > 0}
    Asegura: {res = suma de elementos de t que sean múltiplos de n}
  }
-}
ej_4e_sumarSoloMultiplos (x, y, z) n =
  sum (filter (\i -> not (ej_2h_esMultiploDe i n)) [x, y, z]) -- ? ¿Hay mejor manera de hacer esto?

-- * Ej 4.f

ej_4f_posPrimerPar :: (Integer, Integer, Integer) -> Integer
{-
  problema posPrimerPar(t: (ZxZxZ)): Z
    Requiere: {True}
    Asegura: {res = posición del primer número par en t}
    Asegura: {res = 4 si no hay pares en }
-}
ej_4f_posPrimerPar (x, y, z)
  | ej_2h_esMultiploDe x 2 = 0
  | ej_2h_esMultiploDe y 2 = 1
  | ej_2h_esMultiploDe z 2 = 2
  | otherwise = 4

-- * Ej 4.g

ej_4g_crearPar :: a -> b -> (a, b)
{-
problema crearPar (a, b): (AxB) {
Requiere: {True}
Asegura: {res = (a, b)}
}
-}
ej_4g_crearPar x y = (x, y)

-- * Ej 4.h

ej_4h_invertir :: (a, b) -> (b, a)
{-
  problema invertir (d:(AxB)): (BxA) {
    Requiere: {True}
    Asegura: {res = (snd(d), fst(d)}
  }
-}
ej_4h_invertir (x, y) = (y, x)

-- * Ej 4.i

type Punto2D = (Float, Float)

ej_4i_prodInt :: Punto2D -> Punto2D -> Float
ej_4i_prodInt x1 x2 = fst x1 * fst x2 + snd x1 * snd x2

ej_4i_todoMenor :: Punto2D -> Punto2D -> Bool
ej_4i_todoMenor x1 x2 = fst x1 < fst x2 && snd x1 < snd x2

ej_4i_distanciaPuntos :: Punto2D -> Punto2D -> Float
ej_4i_distanciaPuntos x1 x2 = sqrt ((fst x1 - fst x2) ^ 2 + (snd x1 - snd x2) ^ 2)

-- * Ej 5

ej_5_todosMenores :: (Integer, Integer, Integer) -> Bool
{-
  problema todosMenores (t : Z × Z × Z) : Bool {
    requiere: {True}
    asegura: {(res = true) <=> ((f (t0) > g(t0)) ∧ (f (t1) > g(t1)) ∧ (f (t2) > g(t2)))}
  }
-}
ej_5_todosMenores (x, y, z) = f x > g x && f y > g y && f z > g z
  where
    f :: Integer -> Integer
    f n
      | n <= 7 = n ^ 2
      | n > 7 = 2 * n + 1
    g :: Integer -> Integer
    g n
      | ej_2h_esMultiploDe n 2 = div n 2
      | otherwise = 3 * n + 1

-- * Ej 6

type Anio = Integer

type EsBisiesto = Bool

ej_6_bisiesto :: Anio -> EsBisiesto
{-
  problema bisiesto (año: Z): Bool {
    Requiere: {True}
    Asegura: {res = false <=> año no es múltiplo de 4, o año es múltiplo de 100 pero no de 400}
  }
-}
ej_6_bisiesto a
  | not (ej_2h_esMultiploDe a 4) = False
  | ej_2h_esMultiploDe a 100 && not (ej_2h_esMultiploDe a 400) = False
  | otherwise = True

-- * Ej 7.a

ej_7a_distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
{-
  problema distanciaManhattan (p:(R,R,R), q:(R,R,R)):  R {
    Requiere: {True}
    Asegura: {res = suma de i=0 a 2 de abs(pi - qi)}
  }
-}
ej_7a_distanciaManhattan (x1, y1, z1) (x2, y2, z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

-- * Ej 7.b

type Coordenada3d = (Float, Float, Float)

ej_7b_distanciaManhattan :: Coordenada3d -> Coordenada3d -> Float
ej_7b_distanciaManhattan (x1, y1, z1) (x2, y2, z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
