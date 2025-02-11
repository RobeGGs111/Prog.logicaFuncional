import Data.Text (pack, unpack, replace)
import Text.Numeral.English (uk)

--Función principal que evalúa el número y retorna el resultado
fizzBuzz :: Int -> String
fizzBuzz numero
    | numero `mod` 3 == 0 && numero `mod` 5 == 0 = "FizzBuzz!"
    | numero `mod` 3 == 0 = "Buzz!"
    | numero `mod` 5 == 0 = "Fizz!"
    | otherwise = numeroEnPalabras numero

--Función para convertir números a palabras en inglés
numeroEnPalabras :: Int -> String
numeroEnPalabras numero = capitalizarPrimeraLetra . unpack . replace "-" " " . pack $ uk numero
  where
    capitalizarPrimeraLetra (x:xs) = toUpper x : xs
    capitalizarPrimeraLetra [] = []

main :: IO ()
main = do
    putStrLn "Ingrese un número entre 0 y 100:"
    entrada <- getLine
    let numero = read entrada :: Int
    if numero >= 0 && numero <= 100
        then putStrLn (fizzBuzz numero)
        else putStrLn "Número fuera de rango. Ingrese un número entre 0 y 100."
