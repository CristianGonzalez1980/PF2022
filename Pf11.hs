module Pf11 where

import Pf10
import Pf09
import Pf07
import Pf03 hiding (flip)

--Ejercicio 1) Definir las siguientes funciones utilizando recursión estructural explícita sobre Pizza:
--(Capa (Aceitunas 9) (Capa Queso (Capa Salsa (Capa Jamon (Capa Queso Prepizza))))

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen p Prepizza = 0
cantidadCapasQueCumplen p (Capa i pz) = if p i then 1 + cantidadCapasQueCumplen p pz
                                                else cantidadCapasQueCumplen p pz

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas p Prepizza = Prepizza
conCapasTransformadas p (Capa i pz) = Capa (p i) (conCapasTransformadas p pz) 

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue p Prepizza = Prepizza
soloLasCapasQue p (Capa i pz) = if p i then Capa i (soloLasCapasQue p pz)
                                        else soloLasCapasQue p pz

--Ejercicio 2) Definir las siguientes funciones utilizando alguna de las definiciones anteriores:

sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esQueso)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = (0 ==) . (cantidadCapasQueCumplen esQueso)

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas dupUnidadesAc

--Ejercicio 3) Definir,

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
--que expresa la definición de fold para la estructura de Pizza.
pizzaProcesada f z Prepizza = z
pizzaProcesada f z (Capa i pz) = f i (pizzaProcesada f z pz)

--Ejercicio 4) Resolver todas las funciones de los puntos 1) y 2) utilizando la función pizzaProcesada.
cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' f = pizzaProcesada ((+) . unoSi . f) 0

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (Capa . f) Prepizza 

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f = pizzaProcesada (agregarSi f) Prepizza

agregarSi :: (Ingrediente -> Bool) -> Ingrediente -> (Pizza -> Pizza)
agregarSi f i = if f i then Capa i else id 

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada (agregarSi (not . esQueso)) Prepizza
--sinLactosa' = soloLasCapasQue' (not . esQueso)

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada ((&&) . (not . esQueso)) True
--aptaIntolerantesLactosa' = ((==) 0) . (cantidadCapasQueCumplen' esQueso)

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada ((+) . unoSi . esQueso) 0
--cantidadDeQueso' = cantidadCapasQueCumplen' esQueso 

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (Capa . dupUnidadesAc) Prepizza
--conElDobleDeAceitunas' = conCapasTransformadas' dupUnidadesAc

--Ejercicio 5) Resolver las siguientes funciones utilizando pizzaProcesada (si resulta
--demasiado complejo resolverlas, dar primero una definición por recursión estructural
--explícita, y usar la técnica de los “recuadros”):

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada ((+) . unidadesAc) 0 

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (agregarIngSi f) []

agregarIngSi :: (Ingrediente -> Bool) -> Ingrediente -> ([Ingrediente] -> [Ingrediente])
agregarIngSi f i = if f i then (:) i else id 

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada unificarAc Prepizza

conCapasDe :: Pizza -> Pizza -> Pizza--, que agrega las capas de la primera pizza sobre la segunda
conCapasDe = flip (pizzaProcesada Capa) 

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (pizzaProcesada capasDeArriba (\n -> Prepizza))
                    where capasDeArriba i p = \n -> if n == 0 then Prepizza
                                                                else Capa i (p (n-1))

--Ejercicio 7) Definir las siguientes funciones de esquemas sobre listas, utilizando
--recursión estructural de forma explícita:

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x : filter' f xs
                            else filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs) 

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs)

foldr1' :: (a -> a -> a) -> [a] -> a--caso en funciones como minimo o maximo por ej
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

--(Desafío)
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f z [] = [z] 
scanr' f z (x:xs) = foldr f z (x:xs) : scanr' f z xs

--8 i error de tipado zipWith (curry f) xs ys = map f (zip xs ys)

--Ejercicio 9) Definir las siguientes funciones utilizando solamente ​foldr​:
sum :: [Int] -> Int
sum = foldr (\x b -> x + b) 0
--sum = foldr (+) 0 

length :: [a] -> Int
length = foldr (\x b -> 1 + b) 0
--length = foldr ((+) . (\x -> 1)) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x b -> f x : b) []
--map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x b -> if f x then x : b 
                                    else b) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x b -> if f x then Just x
                                    else b) Nothing 

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\x b -> f x || b) False

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = foldr (\x b -> f x && b) True

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\x b -> if f x then succ b
                                    else b) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\x (ws, zs) -> if f x then (x : ws, zs)
                                                else (ws, x : zs)) ([],[])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr g (\ys -> [])
            where g x r [] = []
                  g x r (y:ys) = f x y : r ys  

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\x r -> if f x then x : r else []) []

take :: Int -> [a] -> [a]
take = flip (foldr (\x r n -> if n == 0 then [] else x : r (n-1)) (\n -> []))

drop :: Int -> [a] -> [a]
drop = flip (foldr (\x r n -> if n == 0 then x : r n else r (n-1)) (\n -> []))

(!!) :: Int -> [a] -> a
(!!) = flip (foldr (\x r n -> if n == 0 then x else r (n-1)) (\n -> error "no existe la posicion"))

twice f x = f (f x)


--Ejercicio 10) Indicar cuáles de las siguientes expresiones tienen tipo, y para aquellas
--que lo tengan, decir cuál es ese tipo:

{--

filter id
SI :: [Bool] -> [Bool]

map (\x y z -> (x, y, z))
SI :: [a] -> [b -> c -> (a,b,c)]

map (+)
SI :: [Int] -> [Int -> Int]

filter fst
SI :: [(Bool,a)] -> [Bool,a]

filter (flip const (+))
SI :: [Bool] -> [Bool]

map const
SI :: [a] -> [b -> a]

map twice
SI :: [a -> a] -> [a -> a]

foldr twice
SO :: a -> [a -> a] -> a --MMMMM

zipWith fst
SO :: a -> [a -> a] -> a --MMMMM

foldr (\x r z -> (x, z) : r z) (const [])
SI :: [a] -> b -> [(a,b)]
--}