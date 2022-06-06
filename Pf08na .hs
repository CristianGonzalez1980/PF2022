module Pf08na where

--import Pf07


f :: [a] -> b
f [] = 
f (x:xs) = f xs

length :: [a] -> Int
--, que describe la cantidad de elementos de la lista.
length [] = 0
length (x:xs) = 1 + length xs

sum :: [Int] -> Int
--, que describe la suma de todos los elementos de la
--lista.
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
--, que describe el producto entre todos los
--elementos de la lista.
product [] = 1
product (x:xs) = x * product xs

concat :: [[a]] -> [a]
--, que describe la lista resultante de concatenar
--todas las listas que son elementos de la dada.
concat [] = []
concat (xs:xxs) = xs : concat xxs

elem :: Eq a => a -> [a] -> Bool
--, que indica si el elemento dado
--pertenece a la lista.
elem e [] = False
elem e (x:xs) = e == x || elem xs

all :: (a -> Bool) -> [a] -> Bool
--, que indica si todos los
--elementos de la lista cumplen el predicado dado.
all p [] = True
all p (x:xs) = p x && all xs

any :: (a -> Bool) -> [a] -> Bool
--, que indica si algún elemento de
--la lista cumple el predicado dado.
any p [] = False 
any p (x:xs) = p x || any xs

count :: (a -> Bool) -> [a] -> Int
--, que describe la cantidad de
--elementos de la lista que cumplen el predicado dado.
count p [] = 0
count p (x:xs) = if (p x) then 1 + count xs
                        else count xs

subset :: Eq a => [a] -> [a] -> Bool
--que indica si todos los
--elementos de la primera lista se encuentran en la segunda.
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys

(++) :: [a] -> [a] -> [a]
--, que describe el resultado de agregar los
--elementos de la primera lista adelante de los elementos de la segunda.
(++) [] ys = ys
(++) (x:xs) ys = x : (++) xs ys

reverse :: [a] -> [a]
--, que describe la lista que tiene los elementos en
--el orden inverso a la lista dada.
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
--, que describe la lista resultante de
--juntar de a pares los elementos de ambas listas, según la posición que
--comparten en cada
zip [] ys = []
zip xs [] = []  
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a],[b])
--, que describe el par de listas que
--resulta de desarmar la lista dada; la primera componente del resultado se
--corresponde con las primeras componentes de los pares dados, y la segunda
--componente con las segundas componentes de dichos pares.
unzip [] = ([], [])
unzip ((a,b):xs) = (a : fst (unzip xs), b : snd (unzip xs))
