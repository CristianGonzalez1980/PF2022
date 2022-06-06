module Pf11 where

import Pf10
import Pf09
import Pf07

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

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = undefined

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined

--(Desafío)
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = undefined