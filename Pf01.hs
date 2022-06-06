
module PF01 where


--ejercicio 1
--8 expresiones que denoten el numero 4


doble :: Int -> Int
doble x = x + x

cuadruple :: Int -> Int
cuadruple x = 4*x

twice f = g
    where g x = f (f x)
{- 
\x -> 4
\x -> cuadruple 1
cuadruple 1
doble (3-1)
doble (doble 1)
doble (6 `div` 3)
12 `div` 3
1 * 4
-}

--ejercicio 2
--reducir doble (doble 2)

{-

doble (doble 2)
    (por def de doble con x = 2)
doble (2+2)
    (por aritmetica)
doble (4)
    (por def de doble con x = 4)
4 + 4
    (por aritmetica)
8

doble (doble 2)
    (por def de doble con x = (doble 2))
(doble 2) + (doble 2)
    (por def de doble con x = (doble 2))
(2 + 2) + (2 + 2)
    (por aritmetica)
4 + 4
    (por aritmetica)
8

-}


--ejercicio 3
--reducir cuadruple 2 y cuadruple (cuadruple 2)

{-
cuadruple 2
    (por def de cuadruple con x = 2)
4*2
    (por aritmetica)
8                           --solo de esta forma--

cuadruple (cuadruple 2)
    (por def de cuadruple con x = 2)
cuadruple (4*2)
    (por aritmetica)
cuadruple 8
    (por def de cuadruple con x = 8)
4*8
    (por aritmetica)
32

cuadruple (cuadruple 2)
    (por def de cuadruple con x = (cuadruple 2))
4*(cuadruple 2)
    (por def de cuadruple con x = 2)
4*(4*2)
    (por aritmetica)
4*8
    (por aritmetica)
32
-}

--ejercicio 4
--definir triple, succ, sumarDos

triple :: Int -> Int
--triple = (3 *)
triple x = 3*x

succ' :: Int -> Int 
--succ' = (+) 1
succ' x = x+1

sumarDos :: Int -> Int
sumarDos x = succ' (succ' x) 
--sumarDos = (+) 2

--ejercicio 5
--comprobar twice succ = sumarDos

{-
twice succ
    (por def de twice con f=succ)
g n
    (ppio de extensionalidad)
succ (succ n)
    (por def de succ con x=n)
succ (n+1)
    (por def de succ con x=(n+1))
(n+1)+1
    (por asociatividad en la suma)
n+1+1
    (por aritmetica)
n+2

sumarDos
    (ppio de extensionalidad)
sumarDos n
    (por def de sumarDos con x=n)
(+) 2 n
    (rescribio como funcion infija)
2+n
    (por conmutatividad en la suma)
n+2
-}

--ejercicio 6
--3 pares de funciones equivalentes

{-
(succ, (\x->x+1) )
(succ (succ 2), (2+2))
(twice doble, cuadruple)
-}

--ejercicio 7
--reduccion completa ((twice twice) doble) 3

{-
((twice twice) doble) 3
    (def de twice con f = twice)
(g doble) 3 
    (where g x = twice (twice x))
twice (twice doble) 3
    (def de twice con f = (twice doble))
g 3
    (where g x = (twice doble) ((twice doble) x))
(twice doble) ((twice doble) 3))
    (def de twice con f = doble)
g ((twice doble) 3)
    (where g x = doble (doble x))
doble (doble ((twice doble) 3))
    (def de doble con x = (doble ((twice doble) 3)))
(doble ((twice doble) 3)) + (doble ((twice doble) 3))
    (def de doble con x = ((twice doble) 3))
((twice doble) 3) + ((twice doble) 3) + (doble ((twice doble) 3))
    (def de doble con x = ((twice doble) 3))
((twice doble) 3) + ((twice doble) 3) + ((twice doble) 3) + ((twice doble) 3)
    (def twice con f = doble)
(g 3) + ((twice doble) 3) + ((twice doble) 3) + ((twice doble) 3)
    (where g x = doble (doble x))
(doble (doble 3)) + ((twice doble) 3) + ((twice doble) 3) + ((twice doble) 3)
    (def twice con f = doble)
(doble (doble 3)) + (g 3) + ((twice doble) 3) + ((twice doble) 3)
    (where g x = doble (doble x))
(doble (doble 3)) + (doble (doble 3)) + ((twice doble) 3) + ((twice doble) 3)
    (def twice con f = doble)
(doble (doble 3)) + (doble (doble 3)) + (g 3) + ((twice doble) 3)
    (where g x = doble (doble x))
(doble (doble 3)) + (doble (doble 3)) + (doble (doble 3)) + ((twice doble) 3)
    (def twice con f = doble)
(doble (doble 3)) + (doble (doble 3)) + (doble (doble 3)) + (g 3)
    (where g x = doble (doble x))
(doble (doble 3)) + (doble (doble 3)) + (doble (doble 3)) + (doble (doble 3))
    (def doble con x = 3)
(doble (3 + 3)) + (doble (doble 3)) + (doble (doble 3)) + (doble (doble 3))
    (def doble con x = 3)
(doble (3 + 3)) + (doble (3 + 3)) + (doble (doble 3)) + (doble (doble 3))
    (def doble con x = 3)
(doble (3 + 3)) + (doble (3 + 3)) + (doble (3 + 3)) + (doble (doble 3))
    (def doble con x = 3)
(doble (3 + 3)) + (doble (3 + 3)) + (doble (3 + 3)) + (doble (3 + 3))
    (aritmetica)
(doble 6) + (doble (3 + 3)) + (doble (3 + 3)) + (doble (3 + 3))
    (aritmetica)
(doble 6) + (doble 6) + (doble (3 + 3)) + (doble (3 + 3))
    (aritmetica)
(doble 6) + (doble 6) + (doble 6) + (doble (3 + 3))
    (aritmetica)
(doble 6) + (doble 6) + (doble 6) + (doble 6)
    (def doble con x = 6)
6 + 6 + (doble 6) + (doble 6) + (doble 6)
    (def doble con x = 6)
6 + 6 + 6 + 6 + (doble 6) + (doble 6)
    (def doble con x = 6)
6 + 6 + 6 + 6 + 6 + 6 + (doble 6)
    (def doble con x = 6)
6 + 6 + 6 + 6 + 6 + 6 + 6 + 6
    (aritmetica)
48
-}

--ejercicio 8

{-
expresiones lambda equivalentes 
 triple,
 succ,
 sumarDos,
 twice,
 twice twice

\x -> 3*x
\x -> x+1
\x -> succ (succ x)
\f -> \x -> f (f x)
\f -> \x -> f (f (f (f x)))
-}

--ejercicio 9

{-
f x = let (y,z) = (x,x) in y
\x -> \z -> (x,x,z)

f (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b
\(x,y) -> x

f p = case p of (x,y) -> x
\(x,y) -> x

f = \p -> let (x,y) = p in y
\(x,y) -> (x,(x,y))
-}
