module Main where

import Data.Char (toLower)
import Utils

------------------------------------------ 3.1

data Pergunta = Sim | Nao deriving (Show)

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs ps = map pergNum ps

and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' Nao _ = Nao
and' _ Nao = Nao

or' :: Pergunta -> Pergunta -> Pergunta
or' Nao Nao = Nao
or' Sim _ = Sim
or' _ Sim = Sim

not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim

------------------------------------------ 3.2

data Temperatura = Celsius | Farenheit | Kelvin

converterCelsius :: Double -> Temperatura -> Double
converterCelsius temp Celsius = temp
converterCelsius temp Kelvin = temp - 273
converterCelsius temp Farenheit = (5 / 9) * (-) temp 32

converterKelvin :: Double -> Temperatura -> Double
converterKelvin temp Celsius = temp + 273
converterKelvin temp Kelvin = temp
converterKelvin temp Farenheit = (5 / 9) * (-) temp 32 + 273

converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit temp Celsius = (9 / 5) * temp + 32
converterFarenheit temp Kelvin = (9 / 5) * (-) temp 273 + 32
converterFarenheit temp Farenheit = temp

------------------------------------------ 3.3

data Jogada = Pedra | Papel | Tesoura deriving (Eq)

instance Ord Jogada where
  (<=) :: Jogada -> Jogada -> Bool
  (<=) Pedra Papel = True
  (<=) Papel Tesoura = True
  (<=) Tesoura Pedra = True
  (<=) _ _ = False

ex3 :: Jogada -> Jogada -> String
ex3 j1 j2
  | j1 == j2 = "Empate"
  | j1 < j2 = "Jogador 2"
  | otherwise = "Jogador 1"

------------------------------------------ 3.4

contains :: (Eq a) => [a] -> a -> Bool
contains xs a = filter ((==) a) xs /= []

isVogal :: Char -> Bool
isVogal c = contains ['a', 'e', 'i', 'o', 'u'] $ toLower c

ex4 :: String -> String
ex4 str = [x | x <- str, not $ isVogal x]

------------------------------------------ 3.5

data Imperial = Inch | Yard | Foot

converterParaMetros :: Double -> Imperial -> Double
converterParaMetros i Inch = i * 0.0254
converterParaMetros y Yard = y * 0.9144
converterParaMetros f Foot = f * 0.3048

converterParaImperial :: Imperial -> Double -> Double
converterParaImperial Inch m = m / 0.0254
converterParaImperial Yard m = m / 0.9144
converterParaImperial Foot m = m / 0.3048

------------------------------------------ 3.6

data Hemisferio = Norte | Sul

data Estacao = Primavera | Verao | Outono | Inverno deriving (Show)

data Mes
  = Janeiro
  | Fevereiro
  | Marco
  | Abril
  | Maio
  | Junho
  | Julho
  | Agosto
  | Setembro
  | Outubro
  | Novembro
  | Dezembro
  deriving (Enum, Eq, Show)

checaFim :: Mes -> Int
checaFim m
  | m == Fevereiro = 28
  | elem m [Abril, Junho, Setembro, Novembro] = 30
  | otherwise = 31

prox :: Mes -> Mes
prox Dezembro = Janeiro
prox m = succ m

oposto :: Estacao -> Estacao
oposto Inverno = Verao
oposto Primavera = Outono

estacao :: Hemisferio -> Mes -> Estacao
estacao Norte m
  | elem m [Dezembro, Janeiro, Fevereiro] = Inverno
  | elem m [Marco, Abril, Maio] = Primavera
  | elem m [Junho, Julho, Agosto] = Verao
  | elem m [Setembro, Outubro, Novembro] = Outono
estacao Sul m = oposto $ estacao Norte m

------------------------------------------ Main

main :: IO ()
main = do
  title "Ex 3.1"
  printLn $ listPergs [Sim, Nao]
  printLn $ pergNum Sim
  printLn $ pergNum Nao
  printLn $ and' Sim Sim
  printLn $ and' Sim Nao
  printLn $ and' Nao Nao
  printLn $ or' Sim Sim
  printLn $ or' Sim Nao
  printLn $ or' Nao Nao
  printLn $ not' Nao

  title "Ex 3.2"
  printLn $ converterCelsius 32 Farenheit -- 0
  printLn $ converterCelsius 273 Kelvin -- 0
  printLn $ converterCelsius 30 Celsius -- 30
  printLn $ converterFarenheit 0 Celsius -- 32
  printLn $ converterFarenheit 273 Kelvin -- 32
  printLn $ converterFarenheit 30 Farenheit -- 30
  printLn $ converterKelvin 32 Farenheit -- 273
  printLn $ converterKelvin 0 Celsius -- 273
  printLn $ converterKelvin 30 Kelvin -- 30
  title "Ex 3.3"
  printLn $ ex3 Pedra Papel
  printLn $ ex3 Pedra Pedra
  printLn $ ex3 Pedra Tesoura

  title "Ex 3.4"
  printLn $ ex4 "Abacate"

  title "Ex 3.5"
  printLn $ converterParaMetros 40 Inch -- ~1
  printLn $ converterParaMetros 1.1 Yard -- ~1
  printLn $ converterParaMetros 3.3 Foot -- ~1
  printLn $ converterParaImperial Inch 1 -- ~40
  printLn $ converterParaImperial Yard 1 -- ~1.1
  printLn $ converterParaImperial Foot 1 -- ~3.3
  
  title "Ex. 3.6"
  printLn $ checaFim Janeiro
  printLn $ checaFim Fevereiro
  printLn $ checaFim Abril
  printLn $ prox Maio
  printLn $ prox Dezembro
  printLn $ estacao Norte Janeiro
  printLn $ estacao Sul Janeiro

  