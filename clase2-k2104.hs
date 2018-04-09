type LongitudAnio = Int

esAceptableConGanga:: String -> Bool
esAceptableConGanga = entre 10 5.abs.diferencia ganga

entre :: Int -> Int -> Int -> Bool
entre mayor menor valor = mayor > valor && menor < valor

diferencia discurso1 discurso2 = (doble.length) discurso1 - (doble.length) discurso2

ganga = "esto es una ganga"

doble x = 2 * x

--aceptable = entre 20 10


cantidadDeDiasInventado anio 
 | esBisiesto anio = 366
 | even anio = 500 
 | otherwise = 365

f 0 = 1500
f x 
 | x > 0 = x*x
 | x < 0 = -x*x

esGrande x = x > 1000000000000 

quieroTenerEsaPlata x = esGrande x && nosearobado

nosearobado = True
 
 
--Estructuras de datos
--Tupla
-- Pattern Matching

data Persona = UnaPersona{
 nombre:: String,
 edad:: Integer,
 casado:: Bool}
  deriving Show

-- edad

--casarse (UnaPersona nombre edad estado) = UnaPersona nombre edad True
casarse fulano = fulano{casado = True}

cumplirAnios fulano = fulano{edad = edad fulano + 1}
--cumplirAnios (UnaPersona nombre edad estado) = UnaPersona nombre (edad + 1) estado 



alguien = UnaPersona "Juan" 130 True
otro = UnaPersona "Maria" 140 False


esMayor :: Persona -> Bool
--esMayor (UnaPersona nombre edad ) = edad > 70
esMayor fulano = edad fulano > 70
 
nombreLargo :: Persona -> Bool
nombreLargo  = (> 4).length.nombre 
--nombreLargo fulano = length (nombre fulano) > 4

esAceptable:: Persona -> Bool
esAceptable fulano = esMayor fulano && nombreLargo fulano

 
--hoy = UnaFecha 5 "abril" 2018

micumpleanios = (12,"mayo",2018)

esDeEsteAnio::(Int,String,Int) -> Bool
esDeEsteAnio (unstring,mes,2018) = True
esDeEsteAnio x = False

 
 
 
 

-- TP
--Queremos saber cuando un año es bisiesto. Lo es, si es divisible entre 4, a menos que sea divisible entre 100. Sin embargo, si un año es divisible entre 100 y además es divisible entre 400, también resulta bisiesto.


divisiblePor :: Int-> Int -> Bool 
divisiblePor nro year = mod year nro == 0 

esBisiesto :: Int -> Bool 
esBisiesto year = (divisiblePor 4 year && not (divisiblePor 100 year)) || (divisiblePor 400 year ) 

--Bonus: Queremos saber cuántos días tiene un año en particular, teniendo en cuenta que los años bisiestos tienen 366 días y el resto tienen 365

cantDias :: Bool -> Int 
cantDias True = 366
cantDias False = 365 

diasDeUnYear year = (cantDias.esBisiesto) year 
--diasDeUnYear = cantDias.esBisiesto

--Variante con guardas
esBisiestoG :: Int -> Bool 
esBisiestoG year 
 | divisiblePor 400 year = True
 | divisiblePor 100 year = False
 | divisiblePor 4 year = True
 | otherwise = False 
