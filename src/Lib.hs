module Lib where
import Text.Show.Functions

laVerdad = True

data Gimnasta = Gimnasta {
nombre :: String,
edad :: Float,
peso :: Float, 
coeficienteTonificacion :: Float 
} deriving(Show)

pancho = Gimnasta "Francisco" 40.0 120.0 1.0

andres = Gimnasta "Andy" 22.0 80.0 6.0

--PUNTO 1--

{-
Saber si alguien está saludable, lo cual se cumple si no está obeso y 
tiene una tonificación mayor a 5. Alguien es obeso si pesa más de 100 kilos. 
> saludable pancho 
False 
> saludable andres 
True
-}

saludable :: Gimnasta -> Bool
saludable gimnasta = (not.esObeso) gimnasta && ((>5).coeficienteTonificacion)  gimnasta

esObeso :: Gimnasta -> Bool
esObeso = (>100).peso

--PUNTO 2--

{-
Hacer que el gimnasta queme una cantidad de calorías, lo que produce que baje de peso.
Si el gimnasta es obeso, baja 1 kilo cada 150 calorías quemadas.
Si no es obeso pero tiene más de 30 años y las calorías quemadas son más de 200, baja siempre un kilo.
En cualquier otro caso se baja la cantidad de calorías quemadas dividido por el producto entre el peso y la edad del gimnasta. 
> quemarCalorias pancho 300
Gimnasta "Francisco" 40.0 118.0 1.0
> quemarCalorias andres 300 
Gimnasta "Andy" 22.0 79.8 6.0
-}

type Calorias = Float

quemarCalorias :: Gimnasta -> Calorias -> Gimnasta
quemarCalorias gimnasta calorias 
 | (esObeso gimnasta) = efectoGimnasta (calorias / 150)  gimnasta calorias 
 | ((not.esObeso) gimnasta) && (((>30).edad) gimnasta) && (calorias > 200) = efectoGimnasta 1  gimnasta calorias 
 | otherwise = efectoGimnasta (calorias / ((peso gimnasta) * (edad gimnasta))) gimnasta calorias 

efectoGimnasta :: Float -> Gimnasta -> Calorias -> Gimnasta
efectoGimnasta numero gimnasta calorias = Gimnasta {nombre = nombre gimnasta, edad = edad gimnasta, peso = (peso gimnasta) - (numero), coeficienteTonificacion = coeficienteTonificacion gimnasta}

--PUNTO 3--

{-
Desarrollar las funciones para los ejercicios caminataEnCinta, entrenamientoEnCinta, pesas, colina y montania sabiendo que:
La cinta quema calorías en función de la velocidad promedio alcanzada durante el ejercicio, quemando 1 caloría por la 
velocidad promedio por minuto.
La caminata es un ejercicio en cinta con velocidad constante de 5 km/h. 
> caminataEnCinta 40 pancho 
Gimnasta "Francisco" 40.0 118.6 1.0 ­­­ --quema 200 calorías (1*5*40) 
El entrenamiento en cinta arranca en 6 km/h y cada 5 minutos incrementa la velocidad en 1 km/h, con lo cual la 
velocidad máxima dependerá de los minutos de entrenamiento.
> entrenamientoEnCinta 40 pancho 
Gimnasta "Francisco" 40.0 117.3 1.0 ­­­ --quema 400 calorías (1* ((6+14)/2) * 40) 
-}

type Velocidad = Float

type Minutos = Float

type Ejercicio = (Gimnasta -> Minutos -> Gimnasta)

type Kilos = Float

type Inclinacion = Float

cinta :: Velocidad -> Ejercicio
cinta velocidad gimnasta minutos = quemarCalorias gimnasta (velocidad * minutos) 

caminataEnCinta = cinta 5

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta  gimnasta minutos = quemarCalorias gimnasta (((5 + 14) / 2) * minutos)

pesas = condicionPesas

condicionPesas :: Kilos -> Ejercicio
condicionPesas kilos gimnasta minutos 
 | (kilos > 10) = tonificar kilos (/10) gimnasta minutos
 | otherwise = gimnasta

tonificar :: Kilos -> (Float -> Float) -> Ejercicio
tonificar kilos operacion gimnasta minutos = Gimnasta {nombre = nombre gimnasta, edad = edad gimnasta, peso = peso gimnasta, coeficienteTonificacion = coeficienteTonificacion gimnasta + (operacion kilos)}


colina :: Inclinacion -> Ejercicio
colina inclinacion gimnasta minutos = quemarCalorias gimnasta (2 * minutos * inclinacion)


montania :: Inclinacion -> Ejercicio
montania inclinacion gimnasta minutos = tonificar 0 (+1) (colina (inclinacion + 3) (colina inclinacion gimnasta (minutos / 2)) (minutos / 2)) minutos


--PUNTO 4--

--A--

data Rutina = Rutina {
nombreRutina :: String,
duracionRutina :: Float,
listaDeEjercicios :: [Ejercicio]
} deriving (Show)

listaDeEjercicioDefault :: [Ejercicio]
listaDeEjercicioDefault = [caminataEnCinta, entrenamientoEnCinta, pesas 50, colina 20, montania 30]

--1)
--Un ejemplo de uso es hacer diferentes tipos de  rutina de ejercicios con nombre para aplicarsela a algun gimnasta--

rutinaFlaca = Rutina "Rutina Flaca" 3 [caminataEnCinta]

rutinaTeHagoVerga = Rutina "Rutina te hago verga" 120 listaDeEjercicioDefault

--2)
--Solucion recursiva

aplicarRutinaRecursivaJugador :: [Ejercicio] -> Gimnasta -> Minutos -> Gimnasta
aplicarRutinaRecursivaJugador [] gimnasta minutos = gimnasta 
aplicarRutinaRecursivaJugador [x] gimnasta minutos = x  gimnasta minutos
aplicarRutinaRecursivaJugador (x:xs) gimnasta minutos = aplicarRutinaRecursivaJugador xs (x gimnasta minutos) minutos

solucionRecursiva :: Gimnasta -> Rutina -> Gimnasta
solucionRecursiva gimnasta rutina = aplicarRutinaRecursivaJugador (listaDeEjercicios rutina) gimnasta (cantidadDeMinutosDedicadaACadaEjercicio rutina)

--3)
--Solucion con fold

aplicarRutinaJugador :: Rutina -> Gimnasta -> Gimnasta
aplicarRutinaJugador rutina gimnasta = foldr (hacerEjercicio rutina) gimnasta (listaDeEjercicios rutina)

hacerEjercicio :: Rutina -> Ejercicio -> Gimnasta -> Gimnasta
hacerEjercicio rutina ejercicio gimnasta = ejercicio gimnasta (cantidadDeMinutosDedicadaACadaEjercicio rutina)
 
cantidadDeMinutosDedicadaACadaEjercicio rutina = (duracionRutina rutina) / (fromIntegral.length) (listaDeEjercicios rutina)

--B--
type Resumen = (String,Float,Float)

resumenRutina :: Gimnasta -> Rutina -> Resumen
resumenRutina gimnasta rutina = (nombreRutina rutina, ((peso gimnasta) - (peso (aplicarRutinaJugador rutina gimnasta))), abs (((coeficienteTonificacion gimnasta) - (coeficienteTonificacion (aplicarRutinaJugador rutina gimnasta)))))

--PUNTO 5--

--Dada una lista de rutinas, obtener un resumen de todas las que (individualmente) pueden llevar a un gimnasta dado a estar saludable.

resumenListaDeRutinasAplicadasAunGimnasta :: [Rutina] -> Gimnasta -> [Resumen]
resumenListaDeRutinasAplicadasAunGimnasta listaDeRutinas gimnasta = map (resumenRutina gimnasta) listaDeRutinas

filtradoDeRutinas :: [Resumen] -> Gimnasta -> [Resumen]
filtradoDeRutinas listaDeResumenes gimnasta = filter (filtrarResumenesSaludables gimnasta) listaDeResumenes

filtrarResumenesSaludables :: Gimnasta -> Resumen -> Bool
filtrarResumenesSaludables gimnasta resumen = (((peso gimnasta) - (segundo resumen)) > 100) &&  ((trd resumen) > 5) 

trd (a,b,c) = c
segundo (a,b,c) = b

loHaceSaludable :: [Rutina] -> Gimnasta -> [Resumen]
loHaceSaludable listaDeRutinas gimnasta = filtradoDeRutinas (resumenListaDeRutinasAplicadasAunGimnasta listaDeRutinas gimnasta) gimnasta

--THE END--









