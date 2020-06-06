module Lib where
import Text.Show.Functions

laVerdad = True

data Gimnasta = Gimnasta String Float Float Float deriving(Show)

pancho = Gimnasta "Francisco" 40.0 120.0 1.0

andres = Gimnasta "Andy" 22.0 80.0 6.0
