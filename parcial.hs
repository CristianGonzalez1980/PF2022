module Parcial where

import Pf03

data Poder = BolaDe Elemento

data Elemento = Fuego | Agua 

type Eleccion = ([String] -> [String])

data Lanzamiento = Lanzar Eleccion 
                 | EnSecuencia Lanzamiento Lanzamiento 
                 | AlMismoTiempo Lanzamiento Lanzamiento

type Vocabulario = [String]

type LibroDeHechizos = ([String] -> Poder)  

data Sabiduria = S Vocabulario LibroDeHechizos

longZipWith :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
{--
longZipWith f g h [] [] = [] 
longZipWith f g h [] (y:ys) = g y : longZipWith f g h [] ys 
longZipWith f g h (x:xs) [] = f x : longZipWith f g h xs []
longZipWith f g h (x:xs) (y:ys) = f x y : longZipWith f g h xs ys
--}
longZipWith f g h [] = (\ys -> if null ys then [] else map h ys)  
longZipWith f g h (x:xs) = g'
                     where g' []     = g x : longZipWith f g h xs []
                           g' (y:ys) = f x y : longZipWith f g h xs ys

elecciones :: Lanzamiento -> [Eleccion]
elecciones (Lanzar e) = [e]
elecciones (EnSecuencia l1 l2) = elecciones l1 ++ elecciones l2  
elecciones (AlMismoTiempo l1 l2) = elecciones l1 ++ elecciones l2  

poderes :: Sabiduria -> Lanzamiento -> [[Poder]]
poderes (S voc ldH) (Lanzar e) = [[ldH (e voc)]]
poderes sab (EnSecuencia l1 l2) = poderes sab l1 ++ poderes sab l2 
poderes sab (AlMismoTiempo l1 l2) = poderes sab l1 ++ poderes sab l2

normalizar :: Lanzamiento -> Lanzamiento
normalizar (Lanzar e) = Lanzar e
normalizar (EnSecuencia l1 l2) = simpEnSec (normalizar l1) (normalizar l2)
normalizar (AlMismoTiempo l1 l2) = simpAlMT (normalizar l1) (normalizar l2)

simpEnSec :: Lanzamiento -> Lanzamiento -> Lanzamiento
simpEnSec (EnSecuencia ls1 ls2) lz = EnSecuencia ls1 (EnSecuencia ls2 lz)
simpEnSec ls1 ls2 = (EnSecuencia ls1 ls2)

simpAlMT :: Lanzamiento -> Lanzamiento -> Lanzamiento
simpAlMT (AlMismoTiempo ls1 ls2) lz = AlMismoTiempo ls1 (AlMismoTiempo ls2 lz)
simpAlMT ls1 ls2 = (AlMismoTiempo ls1 ls2)

foldL :: (Eleccion -> b) -> (b -> b -> b) -> (b -> b -> b) -> Lanzamiento -> b
foldL f g h (Lanzar e) = f e
foldL f g h (EnSecuencia l1 l2) = g (foldL f g h l1) (foldL f g h l2)
foldL f g h (AlMismoTiempo l1 l2) = h (foldL f g h l1) (foldL f g h l2)

recL :: (Eleccion -> b) -> (Lanzamiento -> Lanzamiento -> b -> b -> b) -> (Lanzamiento -> Lanzamiento -> b -> b -> b) -> Lanzamiento -> b
recL f g h (Lanzar e) = f e
recL f g h (EnSecuencia l1 l2) = g l1 l2 (recL f g h l1) (recL f g h l2)
recL f g h (AlMismoTiempo l1 l2) = h l1 l2 (recL f g h l1) (recL f g h l2)

longZipWith' :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
{--
longZipWith' f g h xs ys = foldr k l xs g h ys
                           where k x r g h [] = g x : r g h []
                                 k x r g h (y:ys) = f x y : r g h ys
                                 l g h [] = []
                                 l g h (y:ys) = h y : r g h ys
--}
longZipWith' f g h = foldr k l
                     where k x r []     = g x : r []
                           k x r (y:ys) = f x y : r ys
                           l []         = []
                           l ys         = map h ys

elecciones' :: Lanzamiento -> [Eleccion]
elecciones' = foldL ((:[]) ) (++) (++)

poderes' :: Sabiduria -> Lanzamiento -> [[Poder]]
poderes' (S voc ldH) = foldL (\e -> [[ldH (e voc)]]) (++) (++)

normalizar' :: Lanzamiento -> Lanzamiento
normalizar' = foldL Lanzar simpEnSec simpAlMT

agrupar :: Int -> [Int] -> [[Int]]
agrupar n [ ] = [ ]
agrupar n (x:xs) = agruparPorN n x (agrupar n xs)  

agruparPorN :: Int -> Int -> [[Int]] -> [[Int]]
agruparPorN n e [] = [[e]]
agruparPorN n e (xs:xxs) = if n == length xs then [e] : (xs:xxs) 
                                                else agregarElemAPrimerLista e (xs:xxs) 

agregarElemAPrimerLista :: Int -> [[Int]] -> [[Int]]
agregarElemAPrimerLista x (xs:xxs) = (x : xs) : xxs  


agrupar2 :: Int -> [Int] -> [[Int]]
agrupar2 0 xs = error "no se puede agrupar por 0"
agrupar2 n xs = subst agruparPorN2 id n xs 

agruparPorN2 :: Int -> Int -> [Int] -> [[Int]]
agruparPorN2 n n2 [] = []
agruparPorN2 n n2 (x:xs) = if n2 > 1 then subst (\xxs xs -> xs : (if null xxs then [] else tail xxs)) (\xxs -> x : (if null xxs then [] else head xxs)) (agruparPorN2 n (n2-1) xs)
                                       else [x] : agruparPorN2 n n xs


--armarListaAgrupada ::  
--PROPIEDAD
--poderes s (normalizar lz) = poderes s lz