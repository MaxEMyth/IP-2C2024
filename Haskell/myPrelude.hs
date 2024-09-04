abs' :: (Num a, Ord a) => a->a
abs' x
 |x>=0 = x
 |otherwise = -x

fst' :: (a, b) -> a
fst' (a, _) = a
snd' :: (a, b) -> b
snd' (_, b) = b

fstT :: (a, b, c) -> a
fstT (a, _, _) = a

sndT :: (a, b, c) -> b
sndT (_, b, _) = b

thdT :: (a, b, c) -> c
thdT (_, _, c) = c

max' :: Ord a => a -> a -> a
max' x y
 | x>=y = x
 | otherwise = y

min' :: Ord a => a -> a -> a
min' x y 
 | x<=y = x
 | otherwise = y

maximum' :: Ord a => [a] -> a
maximum' [] = undefined
maximum' [a] = a
maximum' (x:xs) = max' x (maximum' xs)

minimum' :: Ord a => [a] -> a
minimum' [] = undefined
minimum' [a] = a
minimum' (x:xs) = min' x (minimum' xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
-- ? No sé si es idéntica a foldr.
foldr' f x0 [x] = f x x0
foldr' f x0 (x:xs) = f x (foldr' f x0 xs)
