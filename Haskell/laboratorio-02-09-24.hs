doubleMe :: Integer -> Integer
doubleMe x = (2*x) +11
--doubleMe x = 2*x

f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- * Ej 1.b

g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

-- * Ej 1.c

h :: Integer -> Integer
h = f . g

k :: Integer -> Integer
k = g . f

maximo3 :: Integer -> Integer -> Integer -> Integer
{--
  problema maximo3(a:Z, b:Z, c:Z):Z {
    Requiere: {True}
    Asegura: {res = a <=> a>=b && a>=c)}
    Asegura: {res = b <=> b>=a && b>=c)}
    Asegura: {res = c <=> c>=a && c>=b)}
  }
--}
maximo3 a b c
  | a >= b && a >= c = a
  | b >= a && b >= c = b
  | c >= a && c >= b = c

sumaDistintosv2 :: Integer -> Integer -> Integer -> Integer
sumaDistintosv2 x y z
 | x/=y && x/=z && y/=z = x + y + z
 | x==y && y==z = 0
 | x==y = z
 | y==z = x
 | x==z = y