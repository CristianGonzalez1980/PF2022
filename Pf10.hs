module PF10 where

import Pf09

data NExp = Var Variable
            | NCte Int
            | NBOp NBinOp NExp NExp

data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

{--
f :: NExp -> b
f (Var v) =
f (NCte n) = 
f (NBOp nbinop nexp1 nexp2) = f nexp1 nexp2
--}
--y el TAD Memoria cuya interfaz es la siguiente:

enBlanco :: Memoria
--, que describe una memoria vacía.
cuantoVale :: Variable -> Memoria -> Maybe Int
--, que describe el número asociado a la variable dada en la memoria dada.
recordar :: Variable -> Int -> Memoria -> Memoria
--, la memoria resultante de asociar el número dado a la variable dada en la memoria dada.
variables :: Memoria -> [Variable]
--, que describe las variables que la memoria recuerda.

--EJERCICIO 1

evalNExp :: NExp -> Memoria -> Int
--, que describe el número resultante de evaluar la expresión dada a partir de la memoriam dada.
evalNExp (Var v) m = case (cuantoVale v m) of
                        (Just n) -> N
                        Nothing -> error "no existe la variable en la memoria dada"
evalNExp (NCte n) m = n
evalNExp (NBOp nbinop nexp1 nexp2) m = evalNBinop nbinop (evalNExp nexp1 m) (evalNExp nexp2 m)

evalNBinop :: NBinOp -> Int -> Int -> Int
evalNBinop Add = (+)
evalNBinop Sub = (-)
evalNBinop Mul = (*)
evalNBinop Div = div
evalNBinop Mod = mod
evalNBinop Pow = (^)

cfNExp :: NExp -> NExp
--, que describe una expresión con el mismo significado que la dada, pero simplificada 
--y reemplazando las subexpresiones que no dependan de la memoria por su expresión más sencilla. 
--La resolución debe ser exclusivamente simbólica.
cfNExp (Var v) = Var v
cfNExp (NCte n) = NCte n 
cfNExp (NBOp nbinop nexp1 nexp2) = simpNExp nbinop (cfNExp nexp1) (cfNExp nexp2)

simpNExp :: NBinOp -> NExp -> NExp -> NExp
simpNExp Add (NCte n) (NCte m) = NCte (n + m)
simpNExp Sub (NCte n) (NCte m) = NCte (n - m)
simpNExp Mul (NCte n) (NCte m) = NCte (n * m)
simpNExp Div (NCte n) (NCte m) = NCte (n + m)
simpNExp Mod (NCte n) (NCte m) = NCte (n + m)
simpNExp Pow (NCte n) (NCte m) = NCte (n ^ m)
simpNExp nbinop nexp1 nexp2 =  NBOp nbinop nexp1 nexp2


--EJERCICIO 2

data BExp = BCte Bool
            | Not BExp
            | And BExp BExp
            | Or BExp BExp
            | ROp RelOp NExp NExp
data RelOp = Eq
            | NEq
            | Gt
            | GEq
            | Lt
            | LEq

-- Equal y NotEqual
-- Greater y GreaterOrEqual
-- Lower y LowerOrEqual

{--
f :: BExp -> b
f (BCte b) =
f (Not bexp) = f bexp
f (And bexp1 bexp2) = f bexp1 f bexp2
f (Or bexp1 bexp2) = f bexp1 f bexp2
f (ROp relop nexp1 nexp2) = ...
--}

evalBExp :: BExp -> Memoria -> Bool
--, que describe el booleano que resulta de evaluar la expresión dada a partir de la memoria dada.
evalBExp (BCte b) m = b 
evalBExp (Not bexp) m = not (evalBExp bexp m)
evalBExp (And bexp1 bexp2) m = (evalBExp bexp1 m) && (evalBExp bexp2 m)
evalBExp (Or bexp1 bexp2) m = (evalBExp bexp1 m) || (evalBExp bexp2 m)
evalBExp (ROp relop nexp1 nexp2) m = evalRop relop (evalNExp nexp1 m) (evalNExp nexp2 m) 

evalRop :: RelOp -> Bool -> Bool -> Bool
evalRop Eq = (==)
evalRop NEq = (!=)
evalRop Gt = (>)
evalRop GEq = (>=)
evalRop Lt = (<)
evalRop LEq = (<=)

cfBExp :: BExp -> BExp
--, que describe una expresión con el mismo significado que la dada, pero reemplazando las
--subexpresiones que no dependan de la memoria por su expresión más sencilla. La resolución debe ser exclusivamente ​simbólica.
cfBExp (BCte b) = BCte b
cfBExp (Not bexp) = BCte not (cfBExp bexp)
cfBExp (And bexp1 bexp2) = simpBExp And (cfBExp bexp1) (cfBExp bexp2)
cfBExp (Or bexp1 bexp2) = simpBExp Or (cfBExp bexp1) (cfBExp bexp2)
cfBExp (ROp relop nexp1 nexp2) = ROp relop (cfNExp nexp1) (cfNExp nexp2)

simpBExp :: (BExp -> BExp -> BExp) -> BExp -> BExp -> BExp
simpBExp And (BCte b) (BCte d) = BCte (b && d)
simpBExp Or (BCte b) (BCte d) = BCte (b || d)
simpBExp fconst b d = fconst b d