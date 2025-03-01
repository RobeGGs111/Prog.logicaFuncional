import Data.Map (Map, fromList, mapWithKey)
import Data.Char (toUpper)
import Data.List (sum)
import Data.Maybe (fromMaybe)
import Math.Sqrt

--1) Aplicar descuento e IVA a un precio
aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precio descuento = precio * (1 - descuento / 100)

aplicarIVA :: Float -> Float -> Float
aplicarIVA precio iva = precio * (1 + iva / 100)

aplicarOperacionCesta :: [(String, Float)] -> (Float -> Float -> Float) -> Float -> [(String, Float)]
aplicarOperacionCesta cesta operacion porcentaje = 
    [(producto, operacion precio porcentaje) | (producto, precio) <- cesta]

--2) Aplicar función a una lista
aplicarALista :: (a -> b) -> [a] -> [b]
aplicarALista f xs = [f x | x <- xs]

--3) Obtener longitudes de palabras en una frase
obtenerLongitudes :: String -> Map String Int
obtenerLongitudes frase = fromList [(palabra, length palabra) | palabra <- words frase]

--4) Calificar asignaturas
calificarAsignaturas :: Map String Float -> Map String String
calificarAsignaturas notas = fromList [(map toUpper asignatura, calificacion nota) | (asignatura, nota) <- fromList notas]
  where
    calificacion nota
      | nota >= 95 = "Excelente"
      | nota >= 85 = "Notable"
      | nota >= 75 = "Bueno"
      | nota >= 70 = "Suficiente"
      | otherwise  = "Desempeño insuficiente"

--5) Calcular módulo de un vector
moduloVector :: (Floating a) => [a] -> a
moduloVector vector = sqrt (sum (map (^2) vector))

--6) Identificar valores atípicos
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

stdev :: (Floating a) => [a] -> a
stdev xs = sqrt (sum (map (\x -> (x - m) ^ 2) xs) / fromIntegral (length xs))
  where m = mean xs

valoresAtipicos :: (Fractional a, Ord a) => [a] -> [a]
valoresAtipicos muestra = filter (\x -> abs ((x - m) / s) > 3) muestra
  where
    m = mean muestra
    s = stdev muestra
