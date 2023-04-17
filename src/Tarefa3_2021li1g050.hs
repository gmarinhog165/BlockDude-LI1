{- |
Module      : Tarefa3_2021li1g050
Description : Representação textual do jogo
Copyright   : André Miguel Alves de Carvalho <a100818@alunos.uminho.pt>;
            : Gonçalo Duarte Lopes Marinho Gonçalves <a90969@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g050 where

import LI12122
import Tarefa2_2021li1g050


-- | Esta função define um char para cada peça.
definir :: Peca -> Char
definir Bloco = 'X'
definir Vazio = ' '
definir Caixa = 'C'
definir Porta = 'P'


-- | Esta função define um char para a direção do jogador.
jogadordirecao :: Direcao -> Char
jogadordirecao Este = '>'
jogadordirecao Oeste = '<'


-- | Esta função insere o jogador.
subJogador :: [[Char]] -> Jogador -> [[Char]]
subJogador mapa (Jogador (x,y) d _) = l1 ++ [(c1 ++ [jogadordirecao d] ++ drop 1 c2)] ++ (tail l2)
  where (l1,l2) = (take y mapa, drop y mapa)
        l = head l2
        (c1,c2) = (take x l, drop x l)





-- | Função Queda

-- | Esta função converte um mapa numa lista de coordenadas.
conv2 :: Mapa -> [Coordenadas]
conv2 l = conv $ desconstroiMapa l


-- | Esta função verifica se uma coordenada se encontra por cima da outra.
embaixo :: Coordenadas -> [Coordenadas] -> Bool 
embaixo (x,y) m = (x,y+1) `elem` m


-- | Esta função faz o jogador descer se possivel
quedajog :: Jogador -> Mapa -> Jogador 
quedajog j@(Jogador (x,y) d b) l
  | not (embaixo (x,y) (coordssemPorta l)) = quedajog (Jogador (x,y+1) d b) l
  | otherwise = j

-- | Esta função remove o último elemento de uma lista.
removeultimo :: [a] -> [a]
removeultimo [x] = []
removeultimo (h:t) = h : removeultimo t


-- | Esta função dá as coordenadas de um mapa sem a porta.
coordssemPorta :: Mapa -> [Coordenadas]
coordssemPorta l = conv $ filter (\(x,(a,b)) -> x /= Porta) $ desconstroiMapa l


coordssemCaixa :: Mapa -> [Coordenadas]
coordssemCaixa l = conv $ filter (\(x,(a,b)) -> x /= Caixa) $ desconstroiMapa l


-- | Esta função atribui uma visualição no terminal ao jogo.
instance Show Jogo where
  show (Jogo pecas pls) = let mstr = map (map definir) pecas
                              smstr = subJogador mstr (quedajog pls pecas)
                          in removeultimo (unlines smstr)
