module Pf04 where

{-

a. udiv (x,y) = div x y               PARCIAL

b. udivE (x,0) = error "No puedo dividir por 0"
   udivE (x,y) = div x y             PARCIAL

c. udivH = uncurry div               PARCIAL

d. succ x = x + 1                    TOTAL

e. succH = suma 1                    TOTAL

f. porLaMitad = flip div 2           TOTAL

g. conDieresis 'u' = 'ü'             PARCIAL

h. conDieresisB 'u' = 'ü'
   conDieresisB c = conDieresisB c   PARCIAL     

i. conTildePM 'a' = 'á'              PARCIAL
   conTildePM 'e' = 'é'
   conTildePM 'i' = 'í'
   conTildePM 'o' = 'ó'
   conTildePM 'u' = 'ú'

j. conTildeE c = if esVocal c       PARCIAL
                    then conTildePM c
                    else error "El valor recibido no es vocal"

k. conTilde c = if esVocal c && esMinuscula c       TOTAL
                    then conTildePM c
                    else c


EJERCICIO 2

a, b y c

d y e

g y h


EJERCICIO 3		(ayuda recordar los parentesis implicitos)

twice = \f -> \x -> f (f x)

a. twice doble
   ----- #1)

b. twice doble 2
   ----- #1)

c. twice
   ----- #1)

EJERCICIO 4

twice f = g
    where g x = f (f x)

a. twice doble
   ------------ #1)

b. twice doble 2
   ----------- #1)

c. twice
    #0)

EJERCICIO 5

twice f x = f (f x)

a. twice doble
    #0

b. twice doble 2
   ------------- #1)

c. twice
    #0)


EJERCICIO 6

a. a (EL UNICO VALOR DE TIPO a ES BOTOMM)

b. Int -> a     (NO PUEDE HABER FUNCION TOTAL)
        \x -> if x == 0 then bottom 
                            else bottom (esta figuraba como respuesta en el cuestionario)
						'error "" o undefined'
c. a -> b 
        \x -> [x]
        
        \x -> undef (esta figuraba como respuesta en el cuestionario)

explicaciones adicionales:
(a -> a) \x -> bottom
         \x -> x


-}





