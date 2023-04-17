{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- |
Module      : Tarefa2_2021li1g050
Description : Construção/Desconstrução do mapa
Copyright   : André Miguel Alves de Carvalho <a100818@alunos.uminho.pt>;
            : Gonçalo Duarte Lopes Marinho Gonçalves <a90969@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g050 where

import LI12122
import Data.List


-- | Esta função converte uma lista de peças e coordenadas para uma lista de coordenadas.
conv :: [(Peca, Coordenadas)] -> [Coordenadas]
conv [] = []
conv (((x,(a,b)):y)) = (a,b) : conv y


-- | Esta função dá o maior valor de x de uma lista de coordenadas.
valorX :: [Coordenadas] -> Int
valorX [(x,y)] = (x)
valorX ((a,b):(c,d):y)
    | a > c = valorX ((a,b):y)
    | otherwise = valorX ((c,d):y)


-- | Esta função dá o maior valor de x a partir de uma lista de peças com coordenadas.
valorX2 :: [(Peca, Coordenadas)] -> Int
valorX2 l = valorX (conv l)


-- | Esta função dá o maior valor de y de uma lista de coordenadas.
valorY :: [Coordenadas] -> Int
valorY [(x,y)] = (y)
valorY ((a,b):(c,d):y)
                | b > d = valorY ((a,b):y)
                | otherwise = valorY ((c,d):y)


-- | Esta função dá o maior valor de y a partir de uma lista de peças com coordenadas.
valorY2 :: [(Peca, Coordenadas)] -> Int
valorY2 l = valorY (conv l)


-- | Esta função dá a maior coordenada do mapa.
valorX2Y2 :: [(Peca, Coordenadas)] -> Coordenadas
valorX2Y2 l = ((valorX2 l), (valorY2 l))


-- | Esta função transforma a lista de peças e coordenadas numa lista de vazios.
mapaVazio  :: [(Peca, Coordenadas)] -> [Peca]
mapaVazio [] = []
mapaVazio j = replicate ((valorY2 j +1) * (valorX2 j +1)) (Vazio)


-- | Esta função cira uma lista de vazios com o número de espaços vazios do que existem num mapa.
mapaVazioSemPecas :: [(Peca, Coordenadas)] -> [Peca]
mapaVazioSemPecas l = take x (mapaVazio l)
    where x = length (mapaVazio l) - (length $ map snd l)


-- | Esta função forma listas cuja length == Int fornecido.
divide :: [(Peca, Coordenadas)] -> Int -> [[(Peca, Coordenadas)]]
divide [] _ = []
divide l x = take x l : divide (drop x l) x

-- | Esta função vai acrescentar as coordenadas dos vazios à lista.
replace ::  [(Peca, Coordenadas)] ->  [(Peca, Coordenadas)]
replace m = sortOn (snd) (k++m)
     where 
        cp = [(x,y) | x <- [0..xmax] , y<-[0..ymax]]
        lcm = map snd m
        k = zip (mapaVazioSemPecas m) (cp \\ lcm)
        ymax =  snd $ last $ sortOn (snd) $ map snd m
        xmax =  fst $ last $ sort $ map snd m


-- | Esta função vai fazer lista com as linhas e fazer a sua transposta.
transposta ::  [(Peca, Coordenadas)] -> [[(Peca, Coordenadas)]]
transposta m = transpose $ divide (replace m) (valorY2 m +1)


-- | Esta função vai automatizar a função transposta.
func :: [(Peca, Coordenadas)] -> [[(Peca, Coordenadas)]]
func l = transposta (replace l)


-- | Esta função volta a juntar todas as listas, mantendo a ordem.
lista2 :: [[(Peca, Coordenadas)]] -> [(Peca, Coordenadas)]
lista2 l = concat l


-- | Esta função converte para uma lista de peças.
lista3 :: [(Peca, Coordenadas)] -> [Peca]
lista3 [] = []
lista3 ((x,(a,b)):t) = x : lista3 t


-- | Esta função transforma forma as linhas só de peças agora.
unir :: [[(Peca, Coordenadas)]] -> [[Peca]]
unir l = divide2 (lista3 (lista2 l)) ((valorX2 (lista2 l)) +1)


-- | Esta função divide a lista de peças em linhas.
divide2 :: [Peca] -> Int -> [[Peca]]
divide2 [] _ = []
divide2 l x = take x l : divide2 (drop x l) x


-- | Esta função converte num mapa.
funcgeral :: [(Peca, Coordenadas)] -> [[Peca]]
funcgeral [] = []
funcgeral (x:y) = if x `elem` y then unir (func y) else unir (func (x:y))


-- | Função final que constroi o mapa.
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa pecas = funcgeral pecas


-- | Esta função converte um mapa numa lista de peças e respetiva coordenada.
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa = desconstroi (0,0)


-- | Esta função verifica 1 a 1 se é vazio ou não, dando uma lista com apenas as peças.
desconstroi :: Coordenadas -> Mapa -> [(Peca,Coordenadas)]
desconstroi _ [] = []
desconstroi (x,y) ([]:t) = desconstroi (0,y+1) t
desconstroi (x,y) ((m:n):t)
            | m == Vazio = desconstroi (x+1,y) (n:t)
            | m /= Vazio = (m,(x,y)) : desconstroi (x+1,y) (n:t)

