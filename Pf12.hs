module Pf12 where

import Pf09
import Pf08

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
