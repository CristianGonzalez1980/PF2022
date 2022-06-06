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
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2
--, que describe el número resultante
--de sumar todos los números en el árbol dado.

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2
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
countLeaves EmptyT = 0
countLeaves (NodeT x EmptyT EmptyT) = 1
countLeaves (NodeT x t1 t2) = countLeaves t1 + countLeaves t2
--, que describe la cantidad de
--hojas del árbol dado.

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)
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
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x t1 t2) = agregarActual x (todosLosCaminos t1 ++ todosLosCaminos t2)

--, que describe la lista
--con todos los caminos existentes en el árbol dado.

agregarActual :: a -> [[a]] -> [[a]]
agregarActual e [] = []
agregarActual e (x:xs) = (e : x) : agregarActual e xs

--------------------------------------Ejercicio 4)-------------------

data AppList a = Single a
                | Append (AppList a) (AppList a) deriving Show
{--
f :: AppList a -> b 
f (Single x) = ...
f (Append ap1 ap2) = ... f ap1 f ap2

(Append (Single 5) (Append (Single 5) (Append (Single 8) (Append (Single 1) (Append (Single 0) (Append (Single 3) (Single 7)))))))
--}

lenAL :: AppList a -> Int--, que describe la cantidad de elementos de la lista.
lenAL (Single x) = 1
lenAL (Append ap1 ap2) = (lenAL ap1) + (lenAL ap2)

consAL :: a -> AppList a -> AppList a--, que describe la lista resultante de agregar el elemento dado al principio de la lista dada.
consAL e (Single x) = Append (Single e) (Single x)
consAL e (Append ap1 ap2) = Append (consAL e ap1) ap2
--consAL e (Append ap1 ap2) = Append (Single e) (Append ap1 ap2)

headAL :: AppList a -> a--, que describe el primer elemento de la lista dada.
headAL (Single x) = x
headAL (Append ap1 ap2) = headAL ap1

tailAL :: AppList a -> AppList a--, que describe la lista resultante de quitar el primer elemento de la lista dada.
tailAL (Single x) = error "la representacion no admite lista sin elementos"
tailAL (Append (Single x) ap2) = ap2
tailAL (Append ap1 ap2) = Append (tailAL ap1) ap2

snocAL :: a -> AppList a -> AppList a--, que describe la lista resultante de agregar el elemento dado al final de la lista dada.
snocAL e (Single x) = Append (Single x) (Single e)
snocAL e (Append ap1 ap2) = Append ap1 (snocAL e ap2)
--snocAL e (Append ap1 ap2) = Append ap1 (Append ap2 (Single e))

lastAL :: AppList a -> a--, que describe el último elemento de la lista dada.
lastAL (Single x) = x
lastAL (Append ap1 ap2) = lastAL ap2

initAL :: AppList a -> AppList a--, que describe la lista dada sin su último elemento.
initAL (Single x) = error "la representacion no admite lista sin elementos"
initAL (Append ap1 (Single x)) = ap1
initAL (Append ap1 ap2) = Append ap1 (initAL ap2)

reverseAL :: AppList a -> AppList a--, que describe la lista dada con sus elementos en orden inverso.
reverseAL (Single x) = (Single x)
--reverseAL (Append (Single x) ap2) = snocAL x (reverseAL ap2)
reverseAL (Append ap1 ap2) = Append (reverseAL ap2) (reverseAL ap1)

elemAL :: Eq a => a -> AppList a -> Bool--, que indica si el elemento dado se encuentra en la lista dada.
elemAL e (Single x) = e == x
elemAL e (Append ap1 ap2) = elemAL e ap1 || elemAL e ap2

appendAL :: AppList a -> AppList a -> AppList a--, que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.00
appendAL (Single x) app2 = Append (Single x) app2
appendAL (Append ap1 ap2) app2 = Append ap1 (appendAL ap2 app2)
--NOTA: buscar la manera más eficiente de hacerlo.
--appendAL ap1 ap2 = Append ap1 ap2

appListToList :: AppList a -> [a]--, que describe la representación lineal de la lista dada.
appListToList (Single x) = [x]
--appListToList (Append (Single x) ap2) = x : appListToList ap2                
appListToList (Append ap1 ap2) = appListToList ap1 ++ appListToList ap2                

--------------------------------------Ejercicio 5)-------------------

data QuadTree a = LeafQ a
                | NodeQ (QuadTree a) (QuadTree a) 
                        (QuadTree a) (QuadTree a) deriving Show
data Color = RGB Int Int Int deriving Show
type Image = QuadTree Color
{--
f :: QuadTree a -> b
f (LeafQ a) = b 
f (NodeQ q1 q2 q3 q4) = f q1 f q2 f q3 f q4 

(NodeQ (NodeQ (LeafQ (RGB 3 2 0)) (LeafQ (RGB 5 3 0)) (LeafQ (RGB 5 9 8)) (LeafQ (RGB 56 6 6))) (NodeQ (LeafQ (RGB 9 8 3)) (LeafQ (RGB 9 9 6)) (LeafQ (RGB 98 89 9)) (LeafQ (RGB 9 6 6))) (NodeQ (LeafQ (RGB 9 6 6)) (LeafQ (RGB 6 6 6)) (LeafQ (RGB 6 89 6)) (LeafQ (RGB 6 8 6))) (NodeQ (NodeQ (LeafQ (RGB 3 9 3)) (LeafQ (RGB 3 9 3)) (LeafQ (RGB 3 9 3)) (LeafQ (RGB 3 9 3))) (LeafQ (RGB 3 9 3)) (LeafQ (RGB 3 9 3)) (LeafQ (RGB 3 5 3))))

(NodeQ 
    (NodeQ 
        (LeafQ (RGB 3 2 0))
        (LeafQ (RGB 5 3 0))
        (LeafQ (RGB 5 9 8))
        (LeafQ (RGB 56 6 6)))
    (NodeQ 
        (LeafQ (RGB 9 8 3))
        (LeafQ (RGB 9 9 6))
        (LeafQ (RGB 98 89 9))
        (LeafQ (RGB 9 6 6)))
    (NodeQ 
        (LeafQ (RGB 9 6 6))
        (LeafQ (RGB 6 6 6))
        (LeafQ (RGB 6 89 6))
        (LeafQ (RGB 6 8 6)))
    (NodeQ 
        (NodeQ 
            (LeafQ (RGB 3 9 3))
            (LeafQ (RGB 3 9 3))
            (LeafQ (RGB 3 9 3))
            (LeafQ (RGB 3 9 3))) 
        (LeafQ (RGB 8 6 6))
        (LeafQ (RGB 3 9 3))
        (LeafQ (RGB 3 5 3))))
--}

heightQT :: QuadTree a -> Int--, que describe la altura del árbol dado.
heightQT (LeafQ color) = 0 
heightQT (NodeQ q1 q2 q3 q4) = 1 + (heightQT q1) `max` (heightQT q2) `max` (heightQT q3) `max` (heightQT q4) 

countLeavesQT :: QuadTree a -> Int--, que describe la cantidad de hojas del árbol dado.
countLeavesQT (LeafQ color) = 1 
countLeavesQT (NodeQ q1 q2 q3 q4) = (countLeavesQT q1) + (countLeavesQT q2) + (countLeavesQT q3) + (countLeavesQT q4) 

sizeQT :: QuadTree a -> Int--, que describe la cantidad de constructores del árbol dado.
sizeQT (LeafQ color) = 1 
sizeQT (NodeQ q1 q2 q3 q4) = 1 + (sizeQT q1) + (sizeQT q2) + (sizeQT q3) + (sizeQT q4)

compress :: QuadTree Color -> QuadTree Color--, que describe el árbol resultante de transformar en hoja todos aquellos nodos para los
--que se cumpla que todos los elementos de sus subárboles son iguales.
compress (LeafQ color) = LeafQ color
compress (NodeQ (LeafQ c1) (LeafQ c2) (LeafQ c3) (LeafQ c4)) = if (hojasIguales c1 c2 c3 c4) 
                                                                then LeafQ c1 
                                                                else (NodeQ (LeafQ c1) (LeafQ c2) (LeafQ c3) (LeafQ c4))    
compress (NodeQ q1 q2 q3 q4) = NodeQ (compress q1) (compress q2) (compress q3) (compress q4) 

hojasIguales :: Color -> Color -> Color -> Color -> Bool
hojasIguales (RGB c1 c2 c3) (RGB c4 c5 c6) (RGB c7 c8 c9) (RGB  c10 c11 c12) = numerosIguales c1 c4 c7 c10 
                                                                                && numerosIguales c2 c5 c8 c11 
                                                                                && numerosIguales c3 c6 c9 c12
numerosIguales :: Int -> Int -> Int -> Int -> Bool   
numerosIguales n1 n2 n3 n4 = n1 == n2 && n2 == n3 && n3 == n4
{--
uncompress :: QuadTree a -> QuadTree a--, que describe el árbol resultante de transformar en nodo (manteniendo el dato de la
--hoja correspondiente) todas aquellas hojas que no se encuentren en el nivel de la altura del árbol.
uncompress (LeafQ a) = LeafQ a 
uncompress (NodeQ q1 q2 q3 q4) = uncompress q1 uncompress q2 uncompress q3 uncompress q4 


render :: Image -> Int -> Image--, que describe la imagen dada en el tamaño dado.
--Precondición: el tamaño dado es potencia de 4 y su raíz cuarta es mayor o igual a la altura del árbol dado.
--NOTA: Una imagen tiene tamaño t cuando todas las hojas se encuentran en el nivel ∜t.
--AYUDA: Esta operación es similar a uncompress, pero pudiendo variar la altura del árbol
f (LeafQ a) = b 
f (NodeQ q1 q2 q3 q4) = f q1 f q2 f q3 f q4 
--}