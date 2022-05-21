module Pf08 where

import Pf05
{--
f :: [a] -> b
f [] = ...
f (x:xs) = ... f xs
--}
length' :: [a] -> Int
--, que describe la cantidad de elementos de la lista.
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: [Int] -> Int
--, que describe la suma de todos los elementos de la
--lista.
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
--, que describe el producto entre todos los
--elementos de la lista.
product' [] = 1
product' (x:xs) = x * product' xs

concat' :: [[a]] -> [a]
--, que describe la lista resultante de concatenar
--todas las listas que son elementos de la dada.
concat' [] = []
concat' (xs:xxs) = xs ++ (concat' xxs)

elem' :: Eq a => a -> [a] -> Bool
--, que indica si el elemento dado
--pertenece a la lista.
elem' e [] = False
elem' e (x:xs) = e == x || elem' e xs

all' :: (a -> Bool) -> [a] -> Bool
--, que indica si todos los
--elementos de la lista cumplen el predicado dado.
all' p [] = True
all' p (x:xs) = p x && all' p xs

any' :: (a -> Bool) -> [a] -> Bool
--, que indica si algún elemento de
--la lista cumple el predicado dado.
any' p [] = False 
any' p (x:xs) = p x || any' p xs

count' :: (a -> Bool) -> [a] -> Int
--, que describe la cantidad de
--elementos de la lista que cumplen el predicado dado.
count' p [] = 0
count' p (x:xs) = if (p x) then 1 + count' p xs
                        else count' p xs

subset' :: Eq a => [a] -> [a] -> Bool
--que indica si todos los
--elementos de la primera lista se encuentran en la segunda.
subset' [] ys = True
subset' (x:xs) ys = elem' x ys && subset' xs ys

(+++) :: [a] -> [a] -> [a]
--, que describe el resultado de agregar los
--elementos de la primera lista adelante de los elementos de la segunda.
(+++) [] ys = ys
(+++) (x:xs) ys = x : (+++) xs ys

reverse' :: [a] -> [a]
--, que describe la lista que tiene los elementos en
--el orden inverso a la lista dada.
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
--, que describe la lista resultante de
--juntar de a pares los elementos de ambas listas, según la posición que
--comparten en cada
zip' [] ys = []
zip' xs [] = []  
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
--, que describe el par de listas que
--resulta de desarmar la lista dada; la primera componente del resultado se
--corresponde con las primeras componentes de los pares dados, y la segunda
--componente con las segundas componentes de dichos pares.
unzip' [] = ([], [])
unzip' ((a,b):xs) = (a : fst (unzip' xs), b : snd (unzip' xs))


----------------------------------------------------------------

data N = Z | S N deriving Show

{--
f :: N -> b 
f Z = ...
f (S n) = ... f n 
--}

evalN :: N -> Int​
--que describe el número representado por el elemento dado.
evalN Z = 0
evalN (S n) = 1 + evalN n 

addN :: N -> N -> N
--que describe la representación unaria de la suma de los números representados por los argumentos. La
--resolución debe ser exclusivamente ​simbólica​, o sea, SIN calcular cuáles son esos números.
addN Z m = m
addN (S n) m = S (addN n m)

prodN :: N -> N -> N​
--que describe la representación unaria del
--producto de los números representados por los argumentos. La
--resolución debe ser exclusivamente ​simbólica.​
prodN Z m = Z --Z neutro de addN a izq
prodN (S n) m = addN (prodN n m) m 

int2N :: Int -> N
--que describe la representación unaria del número dado usando el tipo ​N​.
int2N 0 = Z
int2N n = S (int2N (n-1))


-------------------------------------------------------------------

type NU = [()]

{--
f :: NU -> b
f [] = ...
f (u:us) = ... f us
--}

evalNU :: NU -> Int​
--que describe el número representado por
--el elemento dado.
evalNU [] = 0
evalNU (u:us) = 1 + evalNU us

succNU :: NU -> NU​
--que describe la representación unaria del
--resultado de sumarle uno al número representado por el argumento.
--La resolución debe ser exclusivamente ​simbólica.​
succNU [] = [()]
succNU (u:us) = () : (succNU us)

addNU :: NU -> NU -> NU​
--que describe la representación unaria
--de la suma de los números representados por los argumentos. La
--resolución debe ser exclusivamente ​simbólica.​
addNU [] nu = nu
addNU (u:us) nu = () : (addNU us nu)

nu2n :: NU -> N​
--que describe la representación unaria dada por
--el tipo ​N​ correspondiente al número representado por el argumento.
nu2n [] = Z
nu2n (u:us) = S (nu2n us)

n2nu :: N -> NU​
--que describe la representación unaria dada por
--el tipo ​NU​ correspondiente al número representado por el argumento.
n2nu Z = []
n2nu (S n) = () : (n2nu n)

-------------------------------------------------------------------

type NBin = [DigBin]

{--
f :: [NBin] -> b    
f [] = ...
f (db:dbs) = .. f dbs
--}

evalNB :: NBin -> Int
--​que describe el número representado por el elemento dado.
evalNB nbin = evalNBreverse (reverse nbin)

evalNBreverse :: NBin -> Int
--​que describe el número representado por el elemento dado.
evalNBreverse [] = error "La representacion minima es de un elemento en la lista"
evalNBreverse [db] = (dbAsInt db) * (2 ^ 0)
evalNBreverse (db:dbs) = (dbAsInt db) * (2 ^ (length' dbs)) + evalNBreverse dbs

normalizarNB :: NBin -> NBin​
--que describe la representación
--binaria del número representado por el argumento, pero sin ​“ceros a
--la izquierda”​ (dígitos redundantes).
--OBSERVACIÓN: por la forma de la representación, los “ceros a
--izquierda” aparecen a la derecha de la lista. Entonces la propiedad
--indica que una lista de dígitos normalizada no puede terminar con el
--dígito 0.

normalizarNB nbin = reverse (normalizarNBreverse (reverse nbin))

normalizarNBreverse :: NBin -> NBin​
normalizarNBreverse [] = []
normalizarNBreverse (db:dbs) = quitarPrimerosCeros db dbs

quitarPrimerosCeros :: DigBin -> NBin -> NBin 
quitarPrimerosCeros I nbin = (I : nbin)
quitarPrimerosCeros _ nbin = normalizarNBreverse nbin

succNB :: NBin -> NBin​
--que describe la representación binaria
--normalizada ​del resultado de sumarle uno al número representado
--por el argumento. La resolución debe ser exclusivamente ​simbólica​, y
--no debe utilizar ​normalizarNB​. Se puede suponer como
--precondición que el argumento está normalizado.

--hago un ej [I,O,I,I,O,I] + I = [O,I,I,I,O,I]

succNB [] = [I]
succNB (db:dbs) = sumarYAcarrearSi (dbAsBool db) dbs

sumarYAcarrearSi :: Bool -> NBin -> NBin
sumarYAcarrearSi False dbs = (I : dbs)
sumarYAcarrearSi True dbs = (O : succNB dbs)   

addNB :: NBin -> NBin -> NBin​
--que describe la representación binaria normalizada de la suma de los números
--representados por los argumentos. La resolución debe ser
--exclusivamente ​simbólica,​ y no debe utilizar ​normalizarNB​. Se
--puede suponer como precondición que los argumentos está
--normalizados.

--hago un ej [I,O,I,I,O,I] + [O,I,I,I,O,I] = [I,I,O,I,I,O,I] 

addNB [] db2 = []
addNB db [] = []
addNB (db:dbs) (db2:db2s) = sumar2Nbin db db2 dbs db2s

sumar2Nbin :: DigBin -> DigBin -> NBin -> NBin -> NBin
sumar2Nbin O O nb1 nb2 = (O : (addNB nb1 nb2))
sumar2Nbin I I nb1 nb2 = (O : (succNB(addNB nb1 nb2)))
sumar2Nbin _ _ nb1 nb2 = (I : (addNB nb1 nb2))


nb2n :: NBin -> N​
--que describe la representación unaria dada
--por el tipo ​N correspondiente al número representado por el
--argumento.

--hago un ej [I,I,O,O,I] = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)  

nb2n nbin = int2N (evalNB nbin)

n2nb :: N -> NBin
--que describe la representación binaria
--normalizada dada por el tipo ​NBin correspondiente al número
--representado por el argumento.

--hago un ej S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z) = [I,I,O,I]  

n2nb n = (evalN n)

--n2nb [] = ...
--n2nb (db:dbs) = .. n2nb dbs



-------------------------------------------------
--seccion III

--ej1

data ExpA = Cte Int
            | Suma ExpA ExpA
            | Prod ExpA ExpA

f (Cte n) = ...
f (Suma exp1 exp2) = ... f exp1 f exp2
f (Prod exp1 exp2) = ... f exp1 f exp2

evalExpA :: ExpA -> Int
--​, que describe el número que resulta
--de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA (Cte n) = n
evalExpA (Suma exp1 exp2) = (evalExpA exp1) + (evalExpA exp2)
evalExpA (Prod exp1 exp2) = (evalExpA exp1) * (evalExpA exp2)

simplificarExpA :: ExpA -> ExpA
--​, que describe una
--expresión aritmética con el mismo significado que la dada, pero que
--no tiene sumas del número 0, ni multiplicaciones por 1 o por 0. La
--resolución debe ser exclusivamente ​simbólica.​
simplificarExpA (Cte n) = (Cte n)
simplificarExpA (Suma exp1 exp2) = simpSuma (simplificarExpA exp1) (simplificarExpA exp2)
simplificarExpA (Prod exp1 exp2) = simpProd (simplificarExpA exp1) (simplificarExpA exp2)

simpSuma :: ExpA -> ExpA -> ExpA
simpSuma (Cte 0) exp2 = exp2
simpSuma exp1 (Cte 0) = exp1
simpSuma exp1 exp2 = Suma exp1 exp2

simpProd :: ExpA -> ExpA -> ExpA
simpProd (Cte 1) exp2 = exp2
simpProd exp1 (Cte 1) = exp1
simpProd (Cte 0) _ = Cte 0
simpProd _ (Cte 0) = Cte 0
simpProd exp1 exp2 = Prod (exp1 exp2)

cantidadDeSumaCero :: ExpA -> Int​
--, que describe la cantidad
--de veces que aparece suma cero en la expresión aritmética dada. La
--resolución debe ser exclusivamente ​simbólica.​
cantidadDeSumaCero (Cte n) = 0
cantidadDeSumaCero (Suma exp1 exp2) = (sumarSiAlgunaEsCero exp1 exp2) + (cantidadDeSumaCero exp1) + (cantidadDeSumaCero exp2)
cantidadDeSumaCero (Prod exp1 exp2) = (cantidadDeSumaCero exp1) + (cantidadDeSumaCero exp2)

sumarSiAlgunaEsCero :: ExpA -> ExpA -> Int
sumarSiAlgunaEsCero (Cte 0) _ = 1 
sumarSiAlgunaEsCero _ (Cte 0) = 1 
sumarSiAlgunaEsCero _ _ = 0


--ej2

data ExpS = CteS N
            | SumS ExpS ExpS
            | ProdS ExpS ExpS

f (CteS n) = ...
f (SumS exp1 exp2) = ... f exp1 f exp2
f (ProdS exp1 exp2) = ... f exp1 f exp2

evalES :: ExpS -> Int​
--, que describe el número que resulta de
--evaluar la cuenta representada por la expresión aritmética dada.
evalES (CteS n) = ...
evalES (SumS exp1 exp2) = ... (evalES exp1) (evalES exp2)
evalES (ProdS exp1 exp2) = ... evalES exp1 evalES exp2

es2ExpA :: ExpS -> ExpA​
--, que describe una expresión
--aritmética representada con el tipo ​ExpA​, que tiene el mismo
--significado que la dada.
f (CteS n) = ...
f (SumS exp1 exp2) = ... f exp1 f exp2
f (ProdS exp1 exp2) = ... f exp1 f exp2

expA2es :: ExpA -> ExpS
--​, que describe una expresión
--aritmética representada con el tipo ​ExpS​, que tiene el mismo
f (CteS n) = ...
f (SumS exp1 exp2) = ... f exp1 f exp2
f (ProdS exp1 exp2) = ... f exp1 f exp2
