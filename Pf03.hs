module Pf03 where

--import pf01.hs
swap (x, y) = (y, x)

--ejercicio 1

curry :: ((a,b) -> c) -> a -> b -> c
--curry = \f x y -> f (x, y)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a,b) -> c
--uncurry = \f (x, y) -> f x y
uncurry f (x, y) = f x y

--ejercicio 2

{-
apply f = g
    where g x = f x
-}
apply :: (a -> b) -> a -> b
apply f x = f x

{-
twice f = g
where g x = f (f x)
-}
twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

{-
id = \x -> x
-}
id' :: a -> a
id' x = x

{-
flip f = g
    where g x = h
        h y = (f y) x
-}
flip :: (b -> a -> c) -> a -> b -> c   
flip f x y = f y x

{-
uflip f = g
    where g p = f (swap p)
-}
uflip :: ((b, a) -> c) -> (a, b) -> c
uflip f p = f (swap p)

{-
const = \x -> (\y -> x)
-}
const' :: a -> b -> a
const' x y = x

{-
compose = \f -> (\g -> (\x -> f (g x)))
-}
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

--ejercicio 4

{-

(apply apply) apply :: (a -> b) -> (a -> b)

(twice doble) 2 :: Int

((twice twice) twice) swap :: (a, b) -> (a, b)  

((flip twice) 1) doble :: Int 

-}

--ejercicio 5

{-
a. appDup f = g
where g x = f (x, x)
-}
appDup f x = f (x, x)
{-
b. appFork (f, g) = h
where h x = (f x, g x)
-}
appFork (f, g) x = (f x, g x)
{-
c. appPar​ ​ (f, g) = h
where h (x, y) = (f x, g y)
-}
appPar (f, g) (x, y) = (f x, g y)

{-
d. appDist f = g
where g (x, y) = (f x, f y)
-}
appDist  f (x, y) = (f x. f y)

{-
e. subst f = h
where h g = k
where k x = (f x) (g x)
-}
subst f g x = f x (g x) 

--ejercicio 6

{-
compose (fst snd) \: (compose fst snd) :: (a, (b, c)) -> b
(uncurry curry snd) \: uncurry (curry snd) :: (a, b) -> b
(apply id) ((id apply) apply) :: (a -> b) -> a -> b
compose (compose doble doble) :: (a -> Int) -> a -> Int
(compose compose) doble doble \: ((a -> Int) -> a -> Int ERROR DE INSTANCIA CHEQUEAR
-}

--compose (fst snd)
{-
compose
(fst snd)
--------------------------
compose (fst snd)

fst ((c, (d, e)) -> (d, e)) -> XX no tipa
snd (a, b) -> b
--------------------------
fst snd

compose :: ((a, b) -> a) -> (c -> (a, b)) -> c -> a   
fst :: (a, b) -> a
--------------------------
compose fst :: (c -> (a, b)) -> c -> a

compose fst :: ((a, (c, d)) -> (c, d)) -> (a, (c, d)) -> c
snd :: (a, b) -> b
--------------------------
compose fst snd :: (a, (c, d)) -> c

************************************

uncurry :: (a -> b -> b) -> (a, b) -> b
curry snd :: a -> b -> b
----------------------------
uncurry curry snd :: (a, b) -> b

curry :: ((a, b) -> b) -> a -> b -> b
snd :: (a, b) -> b
---------------------------
curry snd :: a -> b -> b

****************************************

(apply id) :: ((a -> b) -> a -> b) -> (a -> b) -> a -> b
(id apply) apply :: (a -> b) -> a -> b 
---------------------------
(apply id) ((id apply) apply) :: (a -> b) -> a -> b


apply :: (a -> a) -> (a -> a)
id :: a -> a
----------------------------
apply id :: a -> a

id :: ((a -> b) -> a -> b) -> (a -> b) -> a -> b
apply :: (a -> b) -> a -> b
-----------------------------
id apply :: (a -> b) -> a -> b

(id apply) :: ((a -> b) -> a -> b) -> (a -> b) -> a -> b
apply :: (a -> b) -> a -> b 
------------------------------
id apply apply  :: (a -> b) -> a -> b

*****************************************

compose :: (Int -> Int) -> (a -> Int) -> a -> Int
(compose doble doble) :: Int -> Int
------------------------------
compose (compose doble doble) :: (a -> Int) -> a -> Int

compose doble :: (Int -> Int) -> Int -> Int
doble :: Int -> Int
-------------------------------
compose doble doble :: Int -> Int

compose :: (Int -> Int) -> (a -> Int) -> a -> Int
doble :: Int -> Int
-------------------------------
compose doble :: (a -> Int) -> a -> Int

compose :: ((b -> c) -> (a -> b) -> a -> c) -> (e -> (b -> c) -> (a -> b)) X
compose :: (b -> c) -> (a -> b) -> a -> c
-------------------------------
compose compose ::

-}

--ejercicio 7

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = compose f (many (n-1) f)

--ejercicio 8

{-
a. (Int -> Int) -> Int -> Int

b. (a -> b -> c) -> (a -> b) -> c

c. (a -> b, c -> d) -> (a, c) -> (b, d)

d. ((a, a) -> b) -> a -> b

e. (a -> b -> c) -> b -> a -> c

f. (a -> b) -> (a, a) -> (b, b)

g. (​ a -> b, a -> c) -> a -> (b, c)

h. (a -> b -> c) -> (a -> b) -> a -> c

i. a -> b -> a

--ejercico 9
-}

doble x = x + x
suma x y = x + y
cuadruple x = doble (doble x)
cuadruple' = compose doble doble

timesTwoPlusThree x = suma (doble x) 3

timesTwoPlusThree' = compose (suma 3) doble

fourTimes f x = f (f (f (f x)))

fourTimes' = twice twice 
