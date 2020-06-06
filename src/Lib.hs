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
saludable gimnasta = (not.esObeso) gimnasta && ((>5).coeficienteTonificacion) gimnasta  

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

