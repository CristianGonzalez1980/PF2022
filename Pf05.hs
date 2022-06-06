module Pf05 where

--import Pf03 hiding (uncurry)

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon

data Helado = Vasito Gusto
		| Cucurucho Gusto Gusto
		| Pote Gusto Gusto Gusto


chocoHelate consH = consH Chocolate

--ejercicio 1
{--
--a. Vasito
Vasito :: Gusto -> Helado 

--b. Chocolate
Chocolate :: Gusto

--c. Cucurucho
Cucurucho :: Gusto -> Gusto -> Helado

--d. Sambayón
Sambayon :: Gusto

--e. Pote
Pote :: Gusto -> Gusto -> Gusto -> Helado

--f. chocoHelate
chocoHelate :: (Gusto -> b) -> b 

--g. chocoHelate Vasito
chocoHelate Vasito :: Helado 

--h. chocoHelate Cucurucho
chocoHelate Cucurucho :: Gusto -> Helado 
{--
chocoHelate (Gusto -> Gusto -> Helado) -> Gusto -> Helado
Cucurucho :: Gusto -> Gusto -> Helado
///////////////////////////////////////
chocoHelate Cucurucho :: Gusto -> Helado
--}
--i. chocoHelate (Cucurucho Sambayon)
chocoHelate (Cucurucho Sambayon) :: Helado

--j. chocoHelate (chocoHelate Cucurucho)
chocoHelate (chocoHelate Cucurucho) :: Helado
{--
chocoHelate :: Gusto -> Helado -> Helado
(chocoHelate Cucurucho) :: Gusto -> Helado
////////////////////////////////////////
chocoHelate (chocoHelate Cucurucho) :: Helado
--}
--k. chocoHelate (Vasito DulceDeLeche)
chocoHelate (Vasito DulceDeLeche) /:
{--
chocoHelate :: (->) ->
(Vasito DulceDeLeche) :: Helado
///////////////////////////////////////
chocoHelate (Vasito DulceDeLeche) ::
--}
--l. chocoHelate Pote
chocoHelate Pote :: Gusto -> Gusto -> Helado

--m. chocoHelate (chocoHelate (Pote Frutilla))
chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

--}
--ejercicio 2

data DigBin = O | I deriving Show

--a. 
dbAsInt :: DigBin -> Int{--, que dado un símbolo que representa un
dígito binario lo transforma en su significado como número.--}
dbAsInt O = 0
dbAsInt I = 1
 
--b. 
dbAsBool :: DigBin -> Bool{--, que dado un símbolo que representa un
dígito binario lo transforma en su significado como booleano.--}
dbAsBool O = False
dbAsBool I = True

--c. 
dbOfBool :: Bool -> DigBin{--, que dado un booleano lo transforma en el
símbolo que representa a ese booleano.--}
dbOfBool False = O
dbOfBool True = I

--d. 
negDB :: DigBin -> DigBin{--, que dado un dígito binario lo transforma en
el otro.--}
negDB O = I
negDB I = O


--ejercicio 3

data DigDec = D0 | D1 | D2 | D3 | D4
			| D5 | D6 | D7 | D8 | D9

{--a.--}
ddAsInt :: DigDec -> Int{--, que dado un símbolo que representa un
dígito decimal lo transforma en su significado como número.--}
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

--b
ddOfInt :: Int -> DigDec{--, que dado un número entre 0 y 9 lo transforma
en el símbolo que representa a ese dígito.--} 
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9

--c.
nextDD :: DigDec -> DigDec{--, que dado un dígito decimal lo transforma
en el siguiente según el orden circular dado en la definición.--} 
nextDD D0 = D1
nextDD D1 = D2
nextDD D2 = D3
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0

--d.
prevDD :: DigDec -> DigDec{--, que dado un dígito decimal lo transforma
en el anterior según el orden circular dado en la definición.--}
prevDD D0 = D9
prevDD D1 = D0 
prevDD D2 = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8


--ejercicio 4

data Medida = Mm Float | Cm Float
			| Inch Float | Foot Float

--a. 
asMm :: Medida -> Medida{--, que dada una medida cualquiera la
transforma en una medida en milímetros que aproxima la dada según la
conversión establecida.--}
asMm (Cm fl) = Mm (fl * 10)
asMm (Inch fl) = Mm (fl * 25.4)
asMm (Foot fl) = Mm (fl * 304.8)
asMm med = med 

--b. 
asCm :: Medida -> Medida{--, que dada una medida cualquiera la
transforma en una medida en centímetros que aproxima la dada según la
conversión establecida.--}
asCm (Mm fl) = Cm (fl * 0.1)
asCm (Inch fl) = Cm (fl * 2.54)
asCm (Foot fl) = Cm (fl * 30.48)
asCm med = med

--c. 
asInch :: Medida -> Medida{--, que dada una medida cualquiera la
transforma en una medida en pulgadas que aproxima la dada según la
conversión establecida.--}
asInch (Mm fl) = Inch (fl * 0.039) 
asInch (Cm fl) = Inch (fl * 0.394) 
asInch (Foot fl) = Inch (fl * 12) 
asInch med = med  

--d. 
asFoot :: Medida -> Medida{--, que dada una medida cualquiera la
transforma en una medida en pies que aproxima la dada según la conversión
establecida.--}
asFoot (Mm fl) = Foot (fl * 0.003) 
asFoot (Cm fl) = Foot (fl * 0.033) 
asFoot (Inch fl) = Foot (fl * 0.083)
asFoot med = med 


--ejercicio 5
{--

--a. 
uncurry Rect :: (Float, Float) -> Shape
--b. 
construyeShNormal (flip Rect 5.0) :: Shape
--c. 
compose (uncurry Rect) swap :: (Float, Float) -> Shape
--d. 
uncurry Cucurucho :: (Gusto, Gusto) -> Helado
--e. 
uncurry Rect swap /: 
--f. 
compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado
--g. 
compose Just :: (a -> b) -> a -> (Maybe b)
--h. 
compose uncurry (Pote Chocolate) /:
--}
--donde:
data Shape = Circle Float | Rect Float Float

construyeShNormal :: (Float -> Shape) -> Shape

construyeShNormal c = c 1.0
	

--ejercicio 6

{--
a
uncurry Rect (5.2, 3.5)
c
compose (uncurry Rect) swap (2.9, 9.3)
d
uncurry Cucurucho (Chocolate, Frutilla)
f
compose uncurry Pote Frutilla (Sambayon, DulceDeLeche)
g
compose Just doble ****es suficiente o tambien deberia aplicar la funcion resultante

--}

--ejercico 7
{--

data MayFail a = Raise Exception | Ok a

data Exception = DivByZero | NotFound | NullPointer
		| Other String

type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Raise e) f h = h e
tryCatch (Ok n) f h = f n

---------------------------------------------------------------------
type Nombre = String
type Sueldo = Int
data Empleado = Empleado Nombre Sueldo --simplificado

sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
sueldoGUIE nombre empleados =
	tryCatch (lookupE nombre empleados)
			mostrarInt
			(\e -> case e of
				NotFound -> ventanaError msgNotEmployee
				_        -> error msgUnexpected)
	where msgNotEmployee = "No es empleado de la empresa"
	      msgUnexpected = "Error inesperado"



mostrarInt :: Int -> GUI Int
mostrarInt n = GUI n

ventanaError :: String -> GUI a
ventanaError s = GUI s

lookupE :: Nombre -> [Empleado] -> MayFail Int
lookupE n empleados = sueldoEmpleado n empleados 

sueldoEmpleado :: Nombre -> [Empleado] -> MayFail Int
sueldoEmpleado n [] = Raise NotFound
sueldoEmpleado n (x:xs) = if (esEmpleado n x)
							then Ok (obtenerSueldo x) 
							else sueldoEmpleado n xs

esEmpleado :: Nombre -> Empleado -> Bool
esEmpleado e (Empleado n s) = e == n

obtenerSueldo :: Empleado -> Sueldo
obtenerSueldo (Empleado n s) = s

data GUI a = a

--}