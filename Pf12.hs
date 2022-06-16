module Pf12 where

import Pf09
import Pf08
import Pf03

{--
data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA ExpA

ej:
(Suma (Prod (Cte 1) (Cte 0)) (Suma (Prod (Cte 5) (Cte 0)) (Cte 4)))
--}

--a
foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA f g h (Cte n) = f n
foldExpA f g h (Suma exp1 exp2) = g (foldExpA f g h exp1) (foldExpA f g h exp2)
foldExpA f g h (Prod exp1 exp2) = h (foldExpA f g h exp1) (foldExpA f g h exp2)

--b
cantidadDeCeros :: ExpA -> Int
--que describe la cantidad de ceros explícitos en la expresión dada.
cantidadDeCeros = foldExpA (\n -> if n == 0 then 1 else 0) (+) (+) 

noTieneNegativosExplicitosExpA :: ExpA -> Bool
--que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosExpA = foldExpA (\n -> n < 0) (||) (||)

simplificarExpA' :: ExpA -> ExpA
--que describe una expresión con el mismo significado que la dada, pero que no tiene
--sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente simbólica.
simplificarExpA' = foldExpA Cte (\e1 e2 -> simpSuma e1 e2) (\e1 e2 -> simpProd e1 e2)

evalExpA' :: ExpA -> Int
--que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA' = foldExpA id (\n1 n2 -> n1 + n2) (\n1 n2 -> n1 * n2)

showExpA :: ExpA -> String
--que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showExpA = foldExpA (\n -> "(Cte "++(show n)++")") (\s1 s2 -> "(Suma "++s1++" "++s2++")") (\s1 s2 -> "(Prod "++s1++" "++s2++")")

--d

recExpA :: (Int -> b) -> (ExpA -> ExpA -> b -> b -> b) -> (ExpA -> ExpA -> b -> b -> b) -> ExpA -> b
recExpA f g h (Cte n) = f n
recExpA f g h (Suma exp1 exp2) = g exp1 exp2 (recExpA f g h exp1) (recExpA f g h exp2)
recExpA f g h (Prod exp1 exp2) = h exp1 exp2 (recExpA f g h exp1) (recExpA f g h exp2)

cantDeSumaCeros :: ExpA -> Int
--que describe la cantidad de constructores de suma con al menos uno de sus hijos constante cero.
cantDeSumaCeros = recExpA (const 0) (\exp1 exp2 n1 n2 -> sumaDeCteCero exp1 exp2 n1 n2) (\exp1 exp2 n1 n2 -> n1 + n2)

sumaDeCteCero :: ExpA -> ExpA -> Int -> Int -> Int
sumaDeCteCero (Cte 0) _ n1 n2 = 1 + n1 + n2
sumaDeCteCero _ (Cte 0) n1 n2 = 1 + n1 + n2
sumaDeCteCero _ _ n1 n2 = n1 + n2

cantDeProdUnos :: ExpA -> Int
--que describe la cantidad de constructores de producto con al menos uno de sus hijos constante uno.
cantDeProdUnos = recExpA (const 0) (\exp1 exp2 n1 n2 -> n1 + n2) (\exp1 exp2 n1 n2 -> sumaDeCteUno exp1 exp2 n1 n2)

sumaDeCteUno :: ExpA -> ExpA -> Int -> Int -> Int
sumaDeCteUno (Cte 1) _ n1 n2 = 1 + n1 + n2
sumaDeCteUno _ (Cte 1) n1 n2 = 1 + n1 + n2
sumaDeCteUno _ _ n1 n2 = n1 + n2

--------------------------------------------------------------------------------------------------------------------

{--data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul
(BOp Mul (BOp Sum (Const 0) (Const 10)) (BOp Sum (BOp Mul (Const 2) (Const 3)) (Const 2)))
--}

--a
foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA f g (Const n) = f n
foldEA f g (BOp binop ea1 ea2) = g binop (foldEA f g ea1) (foldEA f g ea2)

--b
noTieneNegativosExplicitosEA :: EA -> Bool
--que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosEA = foldEA (\n -> n /= 0) (\binop r1 r2 -> r1 && r2)

simplificarEA' :: EA -> EA
--que describe una expresión con el mismo significado que la dada, pero que no tiene sumas del
--número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente simbólica.
simplificarEA' = foldEA Const (\binop r1 r2 -> simpBinop binop r1 r2)

simpBinop :: BinOp -> EA -> EA -> EA
simpBinop Sum (Const 0) ea2 = ea2 
simpBinop Sum ea1 (Const 0) = ea1  
simpBinop Sum ea1 ea2 = BOp Sum ea1 ea2
simpBinop Mul (Const 1) ea2 = ea2
simpBinop Mul ea1 (Const 1) = ea1
simpBinop Mul (Const 0) ea2 = (Const 0)
simpBinop Mul ea1 (Const 0) = (Const 0)
simpBinop Mul ea1 ea2 = BOp Mul ea1 ea2

evalEA' :: EA -> Int
--que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA' = foldEA id (\binop r1 r2 -> evalB binop r1 r2)

showEA :: EA -> String
--que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showEA = foldEA (\n -> "(Const "++show n++")") (\binop r1 r2 -> "(BOp "++show binop++" "++r1++" "++r2++")")

ea2ExpA' :: EA -> ExpA
--que describe una expresión aritmética representada con el tipo ExpA, cuyo significado es el mismo que la dada.
ea2ExpA' = foldEA Cte (\binop r1 r2 -> constExpa binop r1 r2)

constExpa :: BinOp -> ExpA -> ExpA -> ExpA
constExpa Sum exp1 exp2 = Suma exp1 exp2
constExpa Mul exp1 exp2 = Prod exp1 exp2

ea2Arbol' :: EA -> Arbol BinOp Int
--que describe la representación como elemento del tipo ABTree BinOp Int de la expresión aritmética dada.
ea2Arbol' = foldEA Hoja (\binop r1 r2 -> Nodo binop r1 r2)

--------------------------------------------------------------------------------------------------------------------

--data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
--(NodeT 8 (NodeT 9 EmptyT EmptyT) (NodeT 5 EmptyT (NodeT 4 EmptyT EmptyT)))

--a

foldT :: (a -> b -> b -> b) -> b -> Tree a -> b
foldT f z EmptyT = z
foldT f z (NodeT x t1 t2) = f x (foldT f z t1) (foldT f z t2)

--b

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT (\x r1 r2 -> NodeT (f x) r1 r2) EmptyT

sumT :: Tree Int -> Int
sumT = foldT (\x r1 r2 -> x + r1 + r2) 0

sizeT :: Tree a -> Int
sizeT = foldT (\x r1 r2 -> 1 + r1 + r2) 0

heightT :: Tree a -> Int
heightT = foldT (\x r1 r2 -> 1 + max r1 r2) 0

preOrder :: Tree a -> [a]
preOrder = foldT (\x r1 r2 -> x : (r1 ++ r2)) []

inOrder :: Tree a -> [a]
inOrder = foldT (\x r1 r2 -> r1 ++ [x] ++ r2) []

postOrder :: Tree a -> [a]
postOrder = foldT (\x r1 r2 -> r1 ++ r2 ++ [x]) []

mirrorT :: Tree a -> Tree a
mirrorT = foldT (\x r1 r2 -> NodeT x r2 r1) EmptyT

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT (\x r1 r2 -> if (f x) then 1 + r1 + r2 else r1 + r2) 0

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT (\x r1 r2 -> if (f x) then (x : fst r1 ++ fst r2, snd r1 ++ snd r2) else (fst r1 ++ fst r2, x : snd r1 ++ snd r2)) ([],[])

zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT g (\t -> EmptyT) 
             where g x r1 r2 EmptyT = EmptyT
                   g x r1 r2 (NodeT y t1 t2) = NodeT (f x y) (r1 t1) (r2 t2) 

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT (\x r1 r2 -> x : (if length r1 > length r2 then r1 else r2) ) []

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT g []
                  where g x [] [] = [[x]]
                        g x r1 r2 = agregarActual x (r1 ++ r2)

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT (\x r1 r2 -> [x] : concatenarNiveles r1 r2) []

nivelN :: Tree a -> Int -> [a]
nivelN = foldT (\x r1 r2 n -> elemDeLevelN n x (r1 (n-1)) (r2 (n-1))) (\n -> [])

--c

recT :: (a -> Tree a -> Tree a -> b -> b -> b) -> b -> Tree a -> b
recT f z EmptyT = z
recT f z (NodeT x t1 t2) = f x t1 t2 (recT f z t1) (recT f z t2)

--d

insertT :: Ord a => a -> Tree a -> Tree a
--que describe el árbol resultante de insertar el elemento dado en el árbol dado, teniendo en cuenta invariantes de BST.
insertT e = recT g (NodeT e EmptyT EmptyT)
            where g x t1 t2 r1 r2 = if e == x then NodeT x t1 t2 
                                              else if e < x then NodeT x r1 t2 
                                                             else NodeT x t1 r2

caminoHasta :: Ord a => a -> Tree a -> [a]
--que describe el camino hasta el elemento dado en el árbol dado.
--Precondición: existe el elemento en el árbol.
caminoHasta e = recT g (error "el arbol no puede estar vacio")
                where g x t1 t2 r1 r2 = if e == x then [x] 
                                                  else if e < x then x : r1 
                                                                 else x : r2 

-------------------------------------------------------------------------------------------------------------------------

type Record a b = [(a,b)]

type Table a b = [ Record a b ]


select :: (Record a b -> Bool) -> Table a b -> Table a b
--que a partir de la lista de registros dada describe la lista de los registros que cumplen con la condición dada.
select p = foldr (\record r -> if p record then record : r else r) []

project :: (a -> Bool) -> Table a b -> Table a b
--que a partir de la lista de registros dada describe la lista de registros solo con los campos que cumplen la condición dada.
project p = foldr (\record r -> cuyosCamposCumplen p record : r) []

cuyosCamposCumplen :: (a -> Bool) -> Record a b -> Record a b     
cuyosCamposCumplen p = foldr (\(x,y) r -> if p x then (x,y) : r else r) []

{--   version explicita
cuyosCamposCumplen p [] = []
cuyosCamposCumplen p ((x,y):cs) = if p x then (x,y) : cuyosCamposCumplen p cs
                                          else cuyosCamposCumplen p cs
--}

conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
--que describe el predicado que da True solo cuando los dos predicados dados lo hacen.
conjunct p q = subst ((&&) . p)  q

crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--que describe el resultado de aplicar una función a cada elemento del producto cartesiano de las dos listas de registros dadas.
crossWith p = zipWith p

product :: Table a b -> Table a b -> Table a b
--que describe la lista de registros resultante del producto cartesiano combinado de las dos
--listas de registros dadas. Es decir la unión de los campos en los registros del producto cartesiano.
product = undefined
{--
similar :: Record a b -> Record a b
--que describe el registro resultante de descartar datos cuyos campos sean iguales (o sea, el mismo dato asociado al mismo campo).
similar = foldr (\record r -> if estaEn record r then r else record : r) []

estaEn :: (a,b) -> Record a b -> Bool
estaEn (x,y) [] = False
estaEn (x,y) ((w,z):cs) = x == w && y == z || estaEn (x,y) cs
-- = foldr (\(x,y) r -> ) False
--}

data Query a b = Table [Record a b] -- Table (Table a b)
               | Product (Query a b) (Query a b)
               | Projection (a -> Bool) (Query a b)
               | Selection (Record a b -> Bool) (Query a b)

{--
Projection
 (/= "age")
 (Selection
  (\r -> any (\(c,v)-> c == "name"
                    && v == "Edward Snowden") r)
(Table [ [("name", "Edward Snowden"),("age", "29")]
 [("name", "Jason Bourne"), ("age", "40")] ]))
--}


--Ejercicio 6)

data Dir = Left' | Right' | Straight' deriving Show
data Mapa a = Cofre [a]
            | Nada (Mapa a)
            | Bifurcacion [a] (Mapa a) (Mapa a) deriving Show
--(Nada (Bifurcacion ["y","r"] (Cofre ["a", "g", "h"]) (Nada (Bifurcacion ["l","i"] (Cofre ["h","w","x"]) (Cofre ["b","c"])))))

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM f g h (Cofre xs) = f xs
foldM f g h (Nada mp) = g (foldM f g h mp)
foldM f g h (Bifurcacion xs mp1 mp2) = h xs (foldM f g h mp1) (foldM f g h mp2) 

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> Mapa a -> b -> b -> b) -> Mapa a -> b
recM f g h (Cofre xs) = f xs
recM f g h (Nada mp) = g mp (recM f g h mp)
recM f g h (Bifurcacion xs mp1 mp2) = h xs mp1 mp2 (recM f g h mp1) (recM f g h mp2) 

objects :: Mapa a -> [a]
--que describe la lista de todos los objetos presentes en el mapa dado.
objects = foldM id id (\xs r1 r2 -> xs ++ r1 ++ r2)

mapM :: (a -> b) -> Mapa a -> Mapa b
--que transforma los objetos del mapa dado aplicando la función dada.
mapM f = foldM (Cofre . (map f)) id (\xs r1 r2 -> Bifurcacion (map f xs) r1 r2)  

has :: (a -> Bool) -> Mapa a -> Bool
--que indica si existe algún objeto que cumpla con la condición dada en el mapa dado.
has p = foldM (any p) id (\xs b1 b2 -> (||) ((||) (any p xs) b1) b2)

hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool
--que indica si un objeto al final del camino dado cumple con la condición dada en el mapa dado.
hasObjectAt p = foldM g h k
                where g xs [] = any p xs
                      g _ _ = error "no hay mas camino para recorrer"
                      h _ [] = False
                      h r (Straight':ds) = r ds 
                      h _ _ = error "no hay camino en la direccion indicada"
                      k xs r1 r2 [] = any p xs
                      k _ r1 r2 (Left':ds) = r1 ds
                      k _ r1 r2 (Right':ds) = r2 ds
                      k _ _ _ _ = error "no hay camino en la direccion indicada"

longestPath :: Mapa a -> [Dir]
--que describe el camino más largo en el mapa dado.
longestPath = foldM (\xs -> []) (\r -> Straight' : r) (\xs r1 r2 -> if length r1 > length r2 then Left' : r1 else Right' : r2)

objectsOfLongestPath :: Mapa a -> [a]
--que describe la lista con los objetos presentes en el camino más largo del mapa dado.
objectsOfLongestPath = recM id (\mp r -> r) (\xs mp1 mp2 r1 r2 -> if length (longestPath mp1) > length (longestPath mp2) then xs ++ r1 else xs ++ r2)

allPaths :: Mapa a -> [[Dir]]
--que describe la lista con todos los caminos del mapa dado.
allPaths = foldM (\xs -> [[]]) (\r -> map (Straight':) r) (\xs r1 r2 -> (map (Left':) r1) ++ (map (Right':)r2))

objectsPerLevel :: Mapa a -> [[a]]
--que describe la lista con todos los objetos por niveles del mapa dado.
objectsPerLevel = foldM (\xs -> [xs]) (\r -> []:r) (\xs r1 r2 -> xs : concatenarNiveles r1 r2)