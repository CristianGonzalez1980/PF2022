module Pf09 where

import Pf08

data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show

{--
f :: EA -> b
f (Const n) = ...b
f (Bop binop ea1 ea2) = ...binop... f ea1 f ea2  
--}

evalEA :: EA -> Int
evalEA (Const n) = n
evalEA (BOp binop ea1 ea2) = evalB binop (evalEA ea1) (evalEA ea2)

evalB :: BinOp -> Int -> Int -> Int
evalB Sum n1 n2 = n1 + n2
evalB Mul n1 n2 = n1 * n2

--, que describe el número que resulta de
--evaluar la cuenta representada por la expresión aritmética dada.

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp binop ea1 ea2) = evalOp binop (ea2ExpA ea1) (ea2ExpA ea2) 

evalOp :: BinOp -> ExpA -> ExpA -> ExpA 
evalOp Sum exp1 exp2 = Suma exp1 exp2
evalOp Mul exp1 exp2 = Prod exp1 exp2

--, que describe una expresión aritmética
--representada con el tipo ExpA, cuya estructura y significado son los
--mismos que la dada.

expA2ea :: ExpA -> EA
expA2ea (Cte n) = Const n
expA2ea (Suma exp1 exp2) = BOp Sum (expA2ea exp1) (expA2ea exp2)  
expA2ea (Prod exp1 exp2) = BOp Mul (expA2ea exp1) (expA2ea exp2)  

--, que describe una expresión aritmética
--representada con el tipo EA, cuya estructura y significado son los
--mismos que la dada.

---------EJERCICIO 2----------------------------------------------------------------
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b) deriving Show

{--
f :: Arbol a b -> b
f (Hoja b) = ...
f (Nodo a ar1 ar2) = ...(f ar1) ... (f ar1)

ej (Nodo Mul (Hoja 2) (Nodo Sum (Hoja 5) (Hoja 6)))
--}

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja b) = 1
cantidadDeHojas (Nodo a ar1 ar2) = (cantidadDeHojas ar1) + (cantidadDeHojas ar2)
--, que describe la
--cantidad de hojas en el árbol dado.

cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja b) = 0
cantidadDeNodos (Nodo a ar1 ar2) = 1 + (cantidadDeNodos ar1) + (cantidadDeNodos ar2)
--, que describe la
--cantidad de nodos en el árbol dado.

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja b) = 1
cantidadDeConstructores (Nodo a ar1 ar2) = 1 + (cantidadDeConstructores ar1) + (cantidadDeConstructores ar2)
--, que
--describe la cantidad de constructores en el árbol dado.

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n) = Hoja n
ea2Arbol (BOp binop ea1 ea2) = Nodo binop (ea2Arbol ea1) (ea2Arbol ea2)
--, que describe la
--representación como elemento del tipo Arbol BinOp Int de la
--expresión aritmética dada.

--------------------------------------Ejercicio 3)-------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
{--
f :: Tree a -> b
f EmptyT = ...
f (NodeT x t1 t2) = ...(f t1) ... (f t2)

(NodeT 9 (NodeT 5 (NodeT 3 EmptyT EmptyT) (NodeT 2 EmptyT EmptyT)) (NodeT 13 (NodeT 8 EmptyT EmptyT) (NodeT 1 EmptyT EmptyT)))
--}

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x t1 t2) = x + (sumarT t1) + (sumarT t2)
--, que describe el número resultante
--de sumar todos los números en el árbol dado.

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + (sizeT t1) + (sizeT t2)
--, que describe la cantidad de elementos
--en el árbol dado.

anyT :: (a -> Bool) -> Tree a -> Bool
anyT p EmptyT = False
anyT p (NodeT x t1 t2) = p x || anyT p t1 || anyT p t2
--, que indica si en el
--árbol dado hay al menos un elemento que cumple con el predicado
--dado.

countT :: (a -> Bool) -> Tree a -> Int
countT p EmptyT = 0
countT p (NodeT x t1 t2) = unoSiP (p x) + countT p t1 + countT p t2
--, que describe la
--cantidad de elementos en el árbol dado que cumplen con el predicado
--dado.
unoSiP :: Bool -> Int
unoSiP True = 1
unoSiP _ = 0

countLeaves :: Tree a -> Int
countLeaves EmptyT = 1
countLeaves (NodeT x t1 t2) = countLeaves t1 + countLeaves t2
--, que describe la cantidad de
--hojas del árbol dado.

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + ((heightT t1) `max` (heightT t2))
--, que describe la altura del árbol
--dado.

inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT x t1 t2) = inOrder t1 ++ [x] ++ inOrder t2
--, que describe la lista in order con los
--elementos del árbol dado.

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = [] 
listPerLevel (NodeT x t1 t2) = [x] : concatenarNiveles (listPerLevel t1) (listPerLevel t2)

concatenarNiveles :: [[a]] -> [[a]] ->[[a]]
concatenarNiveles [] ys = ys
concatenarNiveles xs [] = xs
concatenarNiveles (x:xs) (y:ys) = (x ++ y) : concatenarNiveles xs ys  
--, que describe la lista donde
--cada elemento es una lista con los elementos de un nivel del árbol
--dado.

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)
--, que describe un árbol con los
--mismos elemento que el árbol dado pero en orden inverso.

levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN n (NodeT x t1 t2) = elemDeLevelN n x (levelN (n-1) t1) (levelN (n-1) t2)

elemDeLevelN :: Int -> a -> [a] -> [a] -> [a]
elemDeLevelN 0 x ys ws = [x]
elemDeLevelN _ x ys ws = ys ++ ws
--, que describe la lista con los
--elementos del nivel dado en el árbol dado.

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = x : elemDeMasLarga (ramaMasLarga t1) (ramaMasLarga t2)
--, que describe la lista con los
--elementos de la rama más larga del árbol.

elemDeMasLarga :: [a] -> [a] -> [a]
elemDeMasLarga xs ys = if (length xs > length ys) then xs else ys

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = agregarActual x (todosLosCaminos t1) (todosLosCaminos t2)

--, que describe la lista
--con todos los caminos existentes en el árbol dado.

agregarActual :: a -> [[a]] -> [[a]] -> [[a]]
--agregarActual e [] = [[e]]
agregarActual e (x:xs) (y:ys) = (e : x : [e] ++ xs) ++ (e : y : [e] ++ ys)