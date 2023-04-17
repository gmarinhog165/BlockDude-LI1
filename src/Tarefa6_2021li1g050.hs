{- |
Module      : Tarefa6_2021li1g050
Description : Resolução de um puzzle
Copyright   : André Miguel Alves de Carvalho <a100818@alunos.uminho.pt>;
            : Gonçalo Duarte Lopes Marinho Gonçalves <a90969@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}

module Tarefa6_2021li1g050 where

import LI12122

import Tarefa2_2021li1g050
import Tarefa3_2021li1g050
import Tarefa4_2021li1g050
import Data.List


-- | Função final que dá uma possivel solução com o menor numero de movimentos possiveis, tendo 
-- n como limite, se a menor solução possivel for maior, então devolve Nothing.
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo n (Jogo m (Jogador (x,y) d b)) = seqfinal n (Jogo m (quedajog (Jogador (x,y) d b) m))

-- | Conjunto de movimentos possíveis.
mvs :: [Movimento]
mvs = [AndarEsquerda, AndarDireita, Trepar, InterageCaixa]


-- | Esta função verifica se para um dado jogo o movimento é possivel ou nao, tendo em conta que
-- se a direção do jogador mudar mas a posição não, então é True.
boolmove :: Jogo -> Movimento -> Bool 
boolmove jogo@(Jogo l (Jogador (x,y) d b)) m 
     | m == AndarEsquerda && (moveJogador jogo m) == jogo = False 
     | m == AndarDireita && (moveJogador jogo m) == jogo  = False 
     | m == Trepar && (moveJogador jogo m) == jogo = False 
     | m == InterageCaixa && (moveJogador jogo m) == jogo = False 
     | otherwise = True


-- | Esta função aplica a boolmove e fornece uma lista com cada movimento possivel para cada posição.
truemove :: Jogo -> [Movimento] -> [[Movimento]]
truemove _ [] = []
truemove jogo@(Jogo l (Jogador (x,y) d b)) (m:mv)
     | boolmove jogo m = [m] : truemove jogo mv 
     | otherwise = truemove jogo mv


-- | Esta função serve de auxiliar para a nextmoves4, dando uma lista de movimentos, cria o nº de listas
-- correspondentes à lista dada + os novos movimentos possiveis.
nextmoves3 :: Jogo -> [Movimento] ->  [[Movimento]]
nextmoves3 _ [] = []
nextmoves3 jogo m = zipalt7 m (truemove (correrMovimentos jogo m) mvs) 

-- | Esta função vai acrescentado os novos movimentos possiveis aos anteriores, formando novos conjuntos 
-- de movimentos, recebendo agora toda a lista de listas de movimentos em vez de só uma lista de movimentos.
nextmoves4 :: Jogo -> [[Movimento]] -> [[Movimento]]
nextmoves4 j [[x]] = nextmoves3 j [x]
nextmoves4 j [] = []
nextmoves4 jogo (h:t) = nextmoves3 jogo h ++ nextmoves4 jogo t


-- | Esta função verifica se há alguma sequencia verdadeira nos conjuntos até agora originados, se houver 
-- devolve-a, ou seja, se as coordenadas do jogador coincidirem com as coordenadas da porta.
seqcerta :: Jogo -> [[Movimento]] -> [Movimento]
seqcerta _ [] = [] 
seqcerta jogo@(Jogo l (Jogador (x,y) d b)) move@(m:mv)
     | (converte $ (correrMovimentos jogo m)) == (auxPorta $ coordsPorta l) = m
     | otherwise = seqcerta jogo mv 


-- | Esta função vai originar conjuntos de movimentos até encontrar um verdadeiro, devolvendo-o se a 
-- sua length for inferior a n, ou seja, quando a seqcerta dá algo diferente de [], se não, se a length
-- ainda for inferior a n, continua a criar listas de movimentos possiveis.
movesrecur :: Jogo -> Int -> [[Movimento]] -> [Movimento]
movesrecur jogo@(Jogo l (Jogador (x,y) d b)) n moves@(m:mv)
  | seqcerta jogo moves /= [] && length m <= n = seqcerta jogo moves
  | length m > n = []
  | otherwise = movesrecur jogo n (nextmoves4 jogo moves)

-- | Esta função dá o menor conjunto de movimentos possiveis num dado jogo cujo limite é n, sendo
-- necessário definir que a length é diferente de 0 pois a função movesrecur devolve [] quando 
-- nao há nenhum conjunto de movimentos possiveis.
seqfinal :: Int -> Jogo -> Maybe [Movimento]
seqfinal n jogo 
  | length (movesrecur jogo n (truemove jogo mvs)) <= n && length (movesrecur jogo n (truemove jogo mvs)) /= 0 = Just (movesrecur jogo n (truemove jogo mvs))
  | otherwise = Nothing


-- | Esta função converte um jogo em coordenadas.
converte :: Jogo -> Coordenadas
converte (Jogo m (Jogador (x,y) d b)) = (x,y)

-- | Esta função serve para adicionar os novos movimentos possiveis à lista de movimentos dada, 
-- formando novas listas a partir dessa.
zipalt7 :: [a] -> [[a]] -> [[a]]
zipalt7 _ [] = []
zipalt7 x (h:t) = (x ++ h) : zipalt7 x t

-- | Esta função fornece a lista de coordenadas da porta.
coordsPorta :: Mapa -> [Coordenadas]
coordsPorta l = conv $ filter (\(x,(a,b)) -> x == Porta) $ desconstroiMapa l

-- | Esta função converte uma lista de coordenadas num coordenada, pois só existe uma porta.
auxPorta :: [Coordenadas] -> Coordenadas 
auxPorta (h:t) = h


{-| 
     Nesta tarefa o maior problema foi encontrar um método que gerasse uma solução para os mapas,
     tendo começado por criar um bot que ia de encontro a à porta, porém nao conseguia resolver mapas
     mais dificeis que incluissem a interação com caixas ou ter de voltar atrás para as buscar e não
     consegui pensar em nenhuma ideia que o fizesse.

     Deste modo comecei a procurar no google e youtube métodos mais avançados que me pudessem ajudar 
     a pensar numa ideia para esta tarefa e foi quando vi um vídeo sobre o Breadth First Search (bfs),
     [Video onde aprendi sobre o bfs](https://www.youtube.com/watch?v=pcKY4hjDrxk&t=193s). Porém, não 
     sabia como colocar bem isto em prática com árvores então pensei lembrei-me do método que acabei
     por programar.

     Nesta tarefa, o método que adotei foi verificar para cada posição do jogador, quais os movimentos
     possiveis e formar sempre uma nova lista com cada novo movimento possivel. Esta estratégia pareceu 
     funcionar perfeitamente, porém deparei-me com erros de eficiência e acabei por eliminar funções de 
     modo a que pudesse percorrer o código menos vezes e ser mais rápido. 

     Embora esta função funcione a 100%, o facto de gerar tantas listas, quantos mais movimentos tiver
     a solução dum mapa, mais tempo demora, podendo demorar horas para mais mais complexos, pelo que
     algo que me descontenta é a eficiência da função, algo que para já me transcende e não possuo
     conhecimento, ainda, para a melhorar.

     A realização desta função permitiu-me encontrar erros anteriores que pensava nao ter e corrigi-los,
     pelo que penso ter uma programação operacional para o jogo estar a correr do melhor modo.

     Concluindo, na minha opinião a tarefa 6 apesar de não ser a mais trabalhosa devido à aquisição de
     conhecimento ao longo das tarefas que realizei que me facilitaram em muito a programação desta 
     última, acabou por ser a que exigiu mais trabalho para encontrar uma ideia para a programar.

     -}
