module PF02 where 

    first (x,y) = x
    
    apply f = g
        where g x = f x

    twice f = g
        where g x = f (f x)

    doble x = x + x

    swap (x, y) = (y, x)

    uflip f = g
     where g p = f (swap p)
{-

a)

first (x,y) = x
por regla de aplicacion

first :: (a,b) -> a
(x,y) :: (a,b)
------------------------
first (x,y) :: a

o por regla de definicion
first (x,y) :: a
x :: a

******************************

b)

apply f = g
    where g x = f x

apply :: (a -> b) -> (a -> b)
f :: a -> b
-------------------------
apply f :: a -> b

g :: a -> b
x :: a
--------------------------
gx :: b

f :: a -> b
x :: a
---------------------------
fx :: b


******************************

c)

twice f = g
    where g x = f (f x)

twice :: a' -> a' 
f :: a -> a                     a' <- (a -> a)
----------------------------
twice f :: a'

g :: a -> a
x :: a
----------------------------
g x :: a

f :: a -> a
(f x) :: a
-----------------------------
f (f x) :: a

f :: a -> a
x :: a
-----------------------------
f x :: a


******************************

d)

doble x = x + x

doble :: Int -> Int
x :: Int
-----------------------------
doble x :: Int


x :: Int
x :: Int
------------------------------
x + x :: Int


*******************************

e)

swap (x, y) = (y, x)

swap :: (b,a) -> (a,b)
(x,y) :: (b,a)
---------------------------------
swap (x, y) :: (a,b)

(y, x) :: (a,b)


*******************************

f)

uflip f = g
    where g p = f (swap p)

uflip :: ((a,b) -> c) -> ((b,a) -> c) 
f :: (a,b) -> c 
-----------------------------------
uflip f :: (b,a) -> c

g :: (b,a) -> c
p ::(b,a) 
-----------------------------------
g p :: c

f :: (a,b) -> c 
(swap p) :: (a,b)
------------------------------------
f (swap p) :: c:t 

******************************************************

ejercicio 2

apply :: (a' -> b') -> (a' -> b')
first :: (a,b) -> a                 a' = (a,b) y b' = a
---------------------------------
apply first :: (a,b) -> a

first :: (a',b') -> a'   
(swap, uflip) :: ((b,a) -> (a,b), (((a,b) -> c) -> ((b,a) -> c)))   a' = (b,a) -> (a,b) y b' = ((a,b) -> c) -> ((b,a) -> c)) 
---------------------------------
first (swap, uflip) :: (b,a) -> (a,b)

twice :: (a' -> a') -> (a' -> a') 
doble :: (Int -> Int)               a' = Int
---------------------------------
twice doble :: (Int -> Int)

twice :: (a' -> a') -> (a' -> a')
twice :: (a -> a) -> (a -> a)       a' = (a -> a) 
----------------------------------
twice twice :: (a -> a) -> (a -> a)

twice :: (a' -> a') -> (a' -> a')
uflip :: ((a,b) -> c) -> ((b,a) -> c)   a' -> a' = ((a,b) -> c) -> ((b,a) -> c) 
-----------------------------------
twice uflip :: ((b,a) -> c) -> ((a,b) -> c) 

twice :: (a' -> a') -> (a' -> a')
swap :: (b,a) -> (a,b)              a' -> a' = (b,a) -> (a,b) 
-----------------------------------
twice swap :: (a,b) -> (b,a)

uflip :: ((a',b') -> c) -> ((b',a') -> c)
swap :: (b,a) -> (a,b)              c = (a,b) a' = b b' = a 
------------------------------------
uflip swap :: (a,b) -> (a,b)

(twice twice) :: (a' -> a') -> (a' -> a')
swap :: (b,a) -> (a,b)
		a = b		a' -> (a, b) 
				a' -> (b, a)
------------------------------------
(twice twice) swap :: (a, a) -> (a, a)

?????????????????????????????????????????????????????



******************************************************

ejercicio 3

const x = g
    where g y = x
//VII

const :: a -> (b -> a)
x :: a
-----------------
const x :: b -> a

g :: b -> a
y :: b
-----------------
g y :: a

x :: a

appDup f = g
    where g x = f (x, x)
//II

appDup :: ((a, a) -> b) -> (a -> b) 
f :: (a, a) -> b
-------------------
appDup f :: a -> b

g :: a -> b
x :: a
-------------------
g x :: b

f :: (a, a) -> b
(x, x) :: (a, a)
-------------------
f (x, x) :: b


appFork (f, g) = h
    where h x = (f x, g x)
//V

appFork :: ((a -> b), (a -> c)) -> (a -> (b, c))
(f, g) :: ((a -> b), (a -> c))
-----------------------
appFork (f, g) ::

h :: a -> (b, c)
x :: a
-----------------------
h x :: (b, c)

f :: a -> b
x :: a
-----------------------
f x :: b

g :: a -> c
x :: a 
------------------------
g x :: c

f x :: b
g x :: c
------------------------
(f x, g x) :: (b, c)

x :: a


appPar (f, g) = h
    where h (x, y) = (f x, g y)
//I

appPar :: ((a -> c), (b -> d)) -> ((a, b) -> (c, d))
(f, g) :: ((a -> c), (b -> d))
----------------------
appPar (f, g) :: (a, b) -> (c, d)

h :: (a, b) -> (c, d)
(x, y) :: (a, b)
----------------------
h (x, y) :: (c, d)

(f x) :: c
(g y) :: d
----------------------
(f x, g y) :: (c, d)

f :: a -> c
x :: a
-----------------------
f x :: c

g :: b -> d
y :: b
-----------------------
g x :: d


appDist f = g
    where g (x, y) = (f x, f y)
//IV

appDist :: (a -> b) -> (a, a) -> (b, b) 
f :: a -> b
------------------------
appDist f :: (a, a) -> (b, b)

g :: (a, a) -> (b, b)
(x, y) :: (a, a)
------------------------
g (x, y) :: (b, b)

f x :: b
f y :: b
------------------------
(f x, f y) :: (b, b)

f :: a -> b
x :: a
--------------------------
f x :: c

f :: a -> b
y :: a
--------------------------
f y :: b

x :: a

y :: a

flip f = h
    where h x = k
        where k y = (f y) x
//III

flip :: (b -> (a -> c)) -> a -> (b -> c) 
f :: b -> (a -> c)
----------------------------
flip f :: a -> (b -> c) 

h :: a -> (b -> c)
x :: a
----------------------------
h x :: b -> c

k :: b -> c
y :: b
-----------------------------
k y :: c

f :: b -> (a -> c)
y :: b
------------------------------
f y :: a -> c

(f y) :: a -> c
x :: a
------------------------------
(f y) x :: c

x :: a

y :: b


subst f = h
    where h g = k
        where k x = (f x) (g x)
//VI

subst :: (a -> (c -> b)) -> ((a -> c) -> (a -> b)) 
f :: a -> (c -> b)
--------------------
subst f :: (a -> c) -> (a -> b)
S
h :: (a -> c) -> (a -> b)
g :: (a -> c)
--------------------
h g :: a -> b

k :: a -> b
x :: a
---------------------
k x :: b

f :: a -> (c -> b)
x :: a
---------------------
f x :: c -> b

g :: a -> c
x :: a
---------------------
g x :: c

x :: a

EJERCICIO 4)

a. 1 && 2 == 2                      X NO
b. 1 + if 3 < 5 then 3 else 5       SI INT
c. let par = (True, 4)                 
    in (if first par then first par else second par)
                                    X NO
d. (doble doble) 5                  X NO
e. doble (doble 5)                  SI INT
f. twice first                      ???
g. (twice doble) doble              X NO
h. (twice twice) first              ???
i. apply apply                      SI (a -> b) -> (a -> b)

EJERCICIO 5

Bool                    NULL [], TRUE == FALSE 
(Int, Int)              (5, 2), (DOBLE 2, 1+2)
Char -> Int             \c -> 5,
(Int, Char) -> Bool     ?
(Int -> Int) -> Int     ?
(Bool -> Bool, Int)     ?
a -> Bool                \x -> TRUE,   

EJERCICIO 6

a. \p -> let (f, g) = p
            in \x -> (f x, g x)
appFork


b. \f -> (\g -> (\x -> f x (g x))
subst


c. \f -> (\x -> (\y -> (f y) x)
flip


d. \f -> (\px -> let (x, y) = px
                    in (f x, f y))
appDist


e. \x -> (\y -> x)
const


f. \pf -> let (f, g) = pf
            in \px -> let (x, y) = px
                        in (f x, g y)
appPar


g. \f -> (\x -> f (x, x))
appDup


EJERCICIO 7


    appFork (id,id)            \x -> ((id x), (id x))
        \f -> appDup (appDist f)   \f -> \(appDist f) -> (\x -> (appDist f)(x, x)) = ((f x), (f x))
    appDup id                  \x -> id (x, x)
        appDup appFork             \g -> appFork (g, g) = (\x -> (g x, g x))
flip (appDup const)        \x -> (\y -> ((appDup const) y) x) = (const (y, y) x)
const (appDup id)          \y -> (appDup id) = (\x -> id (x, x))

-}
