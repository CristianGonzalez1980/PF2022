module Pf07 where
import Pf05

--I

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla
                    | Jamon | Queso | Salsa deriving Show

instance Eq Ingrediente where
        (==) Jamon Jamon = True
        (==) Queso Queso = True
        (==) Salsa Salsa = True
        (==) (Aceitunas n) (Aceitunas m) = True
        (==) _ _ = False

--        (/=) ing1 ing2 = not (ing1 == ing2)

{--ej 1
regla base: Prepizza es parte de Pizza
regla inductiva: Si p esta en Pizza entonces Capa Ingrediente p esta en Pizza.
--}

{--ej 2
f :: Pizza -> b
f Prepizza = b
f (Capa ing p) = ... ing ... f p
--}
--ej 3

--a.
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

--b.
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa ing p) = unidadesAc ing + cantidadDeAceitunas p

unidadesAc :: Ingrediente -> Int
unidadesAc (Aceitunas n) = n
unidadesAc _ = 0

--c.
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = Capa (dupUnidadesAc ing) (duplicarAceitunas p)

dupUnidadesAc :: Ingrediente -> Ingrediente
dupUnidadesAc (Aceitunas n) = Aceitunas (n*2)
dupUnidadesAc i = i 

--VER PROPIEDAD

--d.
sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = quitarQueso i (sinLactosa p)

quitarQueso :: Ingrediente -> Pizza -> Pizza
quitarQueso Queso p = p
quitarQueso i p = Capa i p

--e.
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa i p) = noEsQueso i && (aptaIntolerantesLactosa p)

noEsQueso :: Ingrediente -> Bool-- usar not (esQueso)
noEsQueso Queso = False
noEsQueso _ = True

--VER PROPIEDAD

--f.
conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = unificarAc i (conDescripcionMejorada p)

unificarAc :: Ingrediente -> Pizza -> Pizza
unificarAc (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
unificarAc i p = Capa i p


-------------------------------------------------
--II

type Nombre = String
data Planilla = Fin | Registro Nombre Planilla deriving Show
data Equipo = Becario Nombre
            | Investigador Nombre Equipo Equipo Equipo deriving Show

{--ej 1
regla base: Fin es parte de Planilla
regla inductiva: Si p esta en Planilla entonces Registro Nombre p esta en Planilla.

regla base: (Becario Nombre) es parte de Equipo
regla inductiva: Si e1, e2 y e3 estan en Equipo entonces Investigador Nombre e1 e2 e3 esta en Equipo.
--}

{--ej 2
f :: Planilla -> b
f Fin = b
f (Registro n p) = ... n ... f p

f :: Equipo -> b
f (Becario n) = b n
f (Investigador n e1 e2 e3) = ... n ... f e1 ... f e2 ... f e3
--}
--ej 3

--a.
largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

--b.
esta :: Nombre -> Planilla -> Bool
esta nombre Fin = False
esta nombre (Registro n p) = esNombre nombre n || esta nombre p

esNombre :: String -> String -> Bool
esNombre n m = n == m

--c.
juntarPlanilla :: Planilla -> Planilla -> Planilla
juntarPlanilla Fin p = p
juntarPlanilla (Registro n p) p2 = Registro n (juntarPlanilla p p2)

--d.
{--
nivelesJerarquicos :: Equipo -> Int--MAL
nivelesJerarquicos (Becario n) = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + nivelesJerarquicos e1 + nivelesJerarquicos e2 + nivelesJerarquicos e3
--}
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n) = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + nivelesJerarquicos e1 `max` nivelesJerarquicos e2 `max` nivelesJerarquicos e3


--e.
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n) = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

--f.
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanilla (juntarPlanilla (planillaDeIntegrantes e1) (planillaDeIntegrantes e2)) (planillaDeIntegrantes e3))


-------------------------------------------------
--III

data Dungeon a =
        Habitacion a
        | Pasaje (Maybe a) (Dungeon a)
        | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) deriving (Show, Eq)

{--ej 2
f :: Dungeon a -> b
f (Habitacion x) = b
f (Pasaje m d) = ... m ... f d
f (Bifurcacion m d1 d2) = ... m ... f d1 ... f d2
--}

--ej 3

cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion x) = 0
cantidadDeBifurcaciones (Pasaje m d) = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion m d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2
{--
cantidadDePuntosInteresantes :: Dungeon a -> Int--MAL
cantidadDePuntosInteresantes (Habitacion x) = 1
cantidadDePuntosInteresantes (Pasaje m d) = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2
--}
cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion x) = 1
cantidadDePuntosInteresantes (Pasaje m d) = uniSiInt m + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = 1uniSiInt m + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2

uniSiInt :: Maybe a -> Int
uniSiInt Nothing = 0
uniSiInt (Just x) = 1

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion x) = 0
cantidadDePuntosVacios (Pasaje m d) = puntoVacio m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = puntoVacio m + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

puntoVacio :: Maybe a -> Int
puntoVacio Nothing = 1
puntoVacio _ = 0

cantidadDePuntosCon :: a -> Dungeon a -> Int
cantidadDePuntosCon element (Habitacion x) = cuentaSiEsBuscado element x
cantidadDePuntosCon element (Pasaje m d) = cuentaSiEsBuscadoMaybe element m + cantidadDePuntosCon element d
cantidadDePuntosCon element (Bifurcacion m d1 d2) = cuentaSiEsBuscadoMaybe element m + cantidadDePuntosCon element d1 + cantidadDePuntosCon element d2

cuentaSiEsBuscado :: Eq a => a -> a -> Int
cuentaSiEsBuscado x y = if (esBuscado x y) then 1
                                                else 0

cuentaSiEsBuscadoMaybe :: Eq a => a -> Maybe a -> Int
cuentaSiEsBuscadoMaybe x (Nothing) = 0
cuentaSiEsBuscadoMaybe x (Just y) = cuentaSiEsBuscado x y

esLineal :: Dungeon a -> Bool
esLineal (Habitacion a) = True
esLineal (Pasaje m d) = esLineal d
esLineal (Bifurcacion m d1 d2) = False

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe element (Habitacion x) = esBuscado element x
llenoDe element (Pasaje m d) = esBuscadoMaybe element m && llenoDe element d
llenoDe element (Bifurcacion m d1 d2) = esBuscadoMaybe element m && llenoDe element d1 && llenoDe elem d2

esBuscado :: Eq a => a -> a -> Bool
esBuscado x y = x == y

esBuscadoMaybe :: a -> Maybe a -> Bool
esBuscadoMaybe x (Nothing) = False
esBuscadoMaybe x (Just y) = esBuscado x y

data Tesoro = Cofre | Oro | Joyas

data VariasCosas a b = Objeto a | Criatura b

data Monstruo = Gargola | Dragon | Troll

