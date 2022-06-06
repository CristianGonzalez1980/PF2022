module Pf10 where

import Pf09 hiding (Mul)

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
type Memoria = [(Variable,Int)]

enBlanco :: Memoria
enBlanco = undefined
--, que describe una memoria vacía.
cuantoVale :: Variable -> Memoria -> Maybe Int
cuantoVale = undefined
--, que describe el número asociado a la variable dada en la memoria dada.
recordar :: Variable -> Int -> Memoria -> Memoria
recordar = undefined
--, la memoria resultante de asociar el número dado a la variable dada en la memoria dada.
variables :: Memoria -> [Variable]
variables = undefined
--, que describe las variables que la memoria recuerda.

--EJERCICIO 1

evalNExp :: NExp -> Memoria -> Int
--, que describe el número resultante de evaluar la expresión dada a partir de la memoriam dada.
evalNExp (Var v) m = case (cuantoVale v m) of
                        (Just n) -> n
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

evalRop :: RelOp -> Int -> Int -> Bool
evalRop Eq = (==)
evalRop NEq = (/=)
evalRop Gt = (>)
evalRop GEq = (>=)
evalRop Lt = (<)
evalRop LEq = (<=)

cfBExp :: BExp -> BExp
--, que describe una expresión con el mismo significado que la dada, pero reemplazando las
--subexpresiones que no dependan de la memoria por su expresión más sencilla. La resolución debe ser exclusivamente ​simbólica.
cfBExp (BCte b) = BCte b
cfBExp (Not bexp) = cfNot (cfBExp bexp)
cfBExp (And bexp1 bexp2) = cfAnd (cfBExp bexp1) (cfBExp bexp2)
cfBExp (Or bexp1 bexp2) = cfOr (cfBExp bexp1) (cfBExp bexp2)
cfBExp (ROp relop nexp1 nexp2) = cfROp relop (cfNExp nexp1) (cfNExp nexp2)

cfNot :: BExp -> BExp
cfNot (BCte d) = BCte (not d)
cfNot bexp = Not bexp

cfAnd :: BExp -> BExp -> BExp
cfAnd (BCte b) bexp2 = if b then bexp2 else BCte False
cfAnd bexp1 bexp2 = And bexp1 bexp2

cfOr :: BExp -> BExp -> BExp
cfOr (BCte b) bexp2 = if b then BCte True else bexp2 
cfOr bexp1 bexp2 = Or bexp1 bexp2

cfROp :: RelOp -> NExp -> NExp -> BExp
cfROp rop (NCte n) (NCte m) = BCte (evalRop rop n m)
cfROp rop n m = ROp rop n m
{--
--Ejercicio 3)

data Programa = Prog Bloque
type Bloque = [Comando]
data Comando = Assign Nombre NExp
                | If BExp Bloque Bloque
                | While BExp Bloque
{--
f :: Comando -> b
f (Assign n ne) =
f (If be b1 b2) =
f (While be bl) = 
--}

evalProg :: Programa -> Memoria -> Memoria
--que describe la memoria resultante de evaluar el programa dado a partir de la memoria dada.
evalProg (Prog bl) m = evalBlq bl m   

evalBlq :: Bloque -> Memoria -> Memoria
--que describe la memoria resultante de evaluar el bloque dado a partir de la memoria dada.
evalBlq [] m = m 
evalBlq (c:cs) m = let m' = evalCom c m
                    in evalBlq cs m'

evalCom :: Comando -> Memoria -> Memoria
--que describe la memoria resultante de evaluar el comando dado a partir de la memoria dada.
evalCom (Assign n ne) m = recordar n (evalNExp ne m) m 
evalCom (If be b1 b2) m = if (evalBExp be m) then evalBlq b1 m else evalBlq b2 m
evalCom (While be bl) m = evalCom (if be (bl ++ [While be bl]) []) m

optimizeCF :: Programa -> Programa
--que describe un programa con el mismo significado que el dado, pero aplicando constant folding
--sobre las expresiones y descartando los comandos que no serán ejecutados.
optimizeCF (Prog bl) = Prog (optimizeBlq bl)

optimizeBlq :: Bloque -> Bloque
optimizeBlq [] = []
optimizeBlq (c:cs) = optimizeCom c : (optimizeBlq cs)

optimizeCom :: Comando -> Comando
optimizeCom (Assign n ne) = Assign n (cfNExp ne)
optimizeCom (If be b1 b2) = If (cfBExp be) (optimizeBlq b1) (optimizeBlq b2)
optimizeCom (While be bl) = While (cfBExp be) (optimizeBlq bl) 
--}
--EJ4
{--
data DirR = Oeste | Este

data ExpR a = Lit a
            | PuedeMover Dir
            | NroBolitas
            | HayBolitas
            | UnOp UOp (ExpR a)
            | BinOp BOp (ExpR a) (ExpR a)

{--
f :: ExpR a -> b 
f (Lit x) =
f (PuedeMover d) =
f NroBolitas =
f HayBolitas =
f (UnOp uop expr1 expr2)
f (BinOp bop expr1 expr2) 
--}

data UOp = No | Siguiente | Previo

data BOp = YTambien | OBien | Mas | Por

type ProgramaR = ComandoR

data ComandoR = Mover DirR
                | Poner
                | Sacar
                | NoOp
                | Repetir (ExpR Int) ComandoR
                | Mientras (ExpR Bool) ComandoR
                | Secuencia ComandoR ComandoR

{--
f :: ComandoR -> b
f (Mover d) =
f Poner =
f Sacar =
f NoOp =
f (Repetir (expr n) com) =
f (Mientras (expr b) com) =
f (Secuencia com1 com2) =
--}

--y el TAD TableroR cuya interfaz es la siguiente:

tableroInicial :: Int -> TableroR{--, que describe un tablero inicial.--}

mover :: DirR -> TableroR -> TableroR{--, que describe el tablero a
partir del tablero dado donde el cabezal se movió hacia la dirección dada.
Esta operación es parcial, pues debe poderse mover en la dirección dada (ver
puedeMover).--}

poner :: TableroR -> TableroR{--, que describe el tablero donde se
agregó una bolita de la celda actual del tablero dado.--}

sacar :: TableroR -> TableroR{--, que describe el tablero donde se
sacó una bolita de la celda actual del tablero dado. Esta operación es parcial,
pues debe haber bolitas en la celda actual (ver hayBolitas).--}

nroBolitas :: TableroR -> Int{--, que describe la cantidad de bolitas
en la celda actual del tablero dado.--}

hayBolitas :: TableroR -> Bool{--, que indica si hay bolitas en la celda
actual del tablero dado.--}

puedeMover :: DirR -> TableroR -> Bool{--, que indica si el cabezal
se puede mover hacia la dirección dada en el tablero dado.--}

boom :: String -> TableroR -> a{--, que describe el resultado de
realizar una operación de consulta inválida sobre un tablero.--}

--Implementar:

evalExpRInt :: ExpR Int -> TableroR -> Int{--, que
describe el número que resulta de evaluar la expresión dada en el
tablero dado
NOTA: en caso de que la expresión no pueda computar un número,
debe ser considerada una operación inválida--}
evalExpRInt (Lit x) t =
evalExpRInt (PuedeMover d) t = oy
evalExpRInt NroBolitas t =
evalExpRInt HayBolitas t =
evalExpRInt (UnOp uop expr1 expr2) t = (evalExpRInt expr1 t) (evalExpRInt expr2 t)
evalExpRInt (BinOp bop expr1 expr2) t = (evalExpRInt expr1 t) (evalExpRInt expr2 t)

evalExpRBool :: ExpR Bool -> TableroR -> Bool,{-- que
describe el booleano que resulta de evaluar la expresión dada en el
tablero dado
NOTA: en caso de que la expresión no pueda computar un booleano,
debe ser considerada una operación inválida--}

expRTieneTipoInt :: ExpR Int -> Bool,{-- que indica si la
expresión dada no falla cuando se ejecuta evalExpRInt--}

expRTieneTipoBool :: ExpR Bool -> Bool,{-- que indica si la
expresión dada no falla cuando se ejecuta evalExpRBool--}

evalR :: ComandoR -> TableroR -> TableroR,{-- que describe
el tablero resultante de evaluar el comando dado en el tablero dado--}

cantSacar :: ComandoR -> Int{--, que describe la cantidad de
constructores Sacar que hay en el comando dado--}

seqN :: Int -> ComandoR -> ComandoR,{-- que describe la
secuencia de comandos que reitera el comando dado la cantidad de
veces dada--}

repeat2Seq :: ComandoR -> ComandoR,{-- que describe el
comando resultante de reemplazar, en el comando dado, todas las
apariciones del constructor Repetir aplicado a un literal por la
secuencia de comandos que reitera el comando correspondiente la
cantidad de veces dada por el literal.--}

--}