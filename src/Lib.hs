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

