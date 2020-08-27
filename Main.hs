module Main where 

import Prelude hiding (Real)

data Mantissa = Empty | One Mantissa | Zero Mantissa deriving (Show)
data Peano = Null | Succ Peano deriving (Show)
data Sign = Plus | Minus deriving (Show)
data Integer = Integer Sign Peano deriving (Show)
data Real = Real Sign Peano Mantissa deriving (Show)

mantissaToBinary :: Mantissa -> String
mantissaToBinary Empty    = ""
mantissaToBinary (Zero m) = "0" ++ mantissaToBinary m
mantissaToBinary (One  m) = "1" ++ mantissaToBinary m

peanoToBinary :: Peano -> String
peanoToBinary Null        = "0"
peanoToBinary (Succ Null) = "1"
peanoToBinary n | peanoToInt n `mod` 2 == 1 = toBin (peanoToInt n `div` 2) ++ "1" 
                | peanoToInt n `mod` 2 == 0 = toBin (peanoToInt n `div` 2) ++ "0"

toBin :: Int -> String
toBin 0 = "0"
toBin 1 = "1"
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ "0" 
    | otherwise = toBin (n `div` 2) ++ "1"

peanoToInt :: Peano -> Int
peanoToInt Null     = 0
peanoToInt (Succ n) = 1 + peanoToInt n

signToString :: Sign -> String 
signToString Plus  = "+"
signToString Minus = "-"

realToString :: Real -> String
realToString (Real sign peano mantissa) = signToString sign ++ peanoToBinary peano ++ "." ++ mantissaToBinary mantissa

mantissaExample1 = One (Zero (Zero (Zero Empty)))
mantissaExample2 = Zero (One (Zero (Zero Empty)))
mantissaExample3 = Zero (Zero (One (Zero Empty)))
mantissaExample4 = Zero (Zero (Zero (One Empty)))

infiniteMantissaExample1 = One (Zero infiniteMantissaExample1)

peanoExample1 = Null
peanoExample2 = Succ Null
peanoExample3 = Succ (Succ Null)
peanoExample4 = Succ (Succ (Succ Null))
peanoExample5 = Succ (Succ (Succ (Succ Null)))

infinitePeano = Succ infinitePeano

realNumberExample1 = Real Plus Null (One (Zero (Zero (Zero Empty))))
realNumberExample2 = Real Minus (Succ (Succ Null)) (Zero (Zero (Zero (One Empty))))

infiniteRealNumberExample = Real Plus infinitePeano infiniteMantissaExample1

main = do
  putStrLn $ show mantissaExample1 ++ " -> " ++ mantissaToBinary mantissaExample1
  putStrLn $ show mantissaExample2 ++ " -> " ++ mantissaToBinary mantissaExample2
  putStrLn $ show mantissaExample3 ++ " -> " ++ mantissaToBinary mantissaExample3
  putStrLn $ show mantissaExample4 ++ " -> " ++ mantissaToBinary mantissaExample4
  putStrLn $ show peanoExample1 ++ " -> " ++ peanoToBinary peanoExample1 
  putStrLn $ show peanoExample2 ++ " -> " ++ peanoToBinary peanoExample2 
  putStrLn $ show peanoExample3 ++ " -> " ++ peanoToBinary peanoExample3 
  putStrLn $ show peanoExample4 ++ " -> " ++ peanoToBinary peanoExample4 
  putStrLn $ show peanoExample5 ++ " -> " ++ peanoToBinary peanoExample5 
  putStrLn $ show realNumberExample1 ++ " -> " ++ realToString realNumberExample1
  putStrLn $ show realNumberExample2 ++ " -> " ++ realToString realNumberExample2
