{- |
Module      : Tarefa1_2021li1g050
Description : Validação de um potencial mapa
Copyright   : André Miguel Alves de Carvalho <a100818@alunos.uminho.pt>;
            : Gonçalo Duarte Lopes Marinho Gonçalves <a90969@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g050 where

import LI12122
import Data.List


-- | Esta função verifica se há coordenadas repetidas.
verificaRepetidos :: [(Peca, Coordenadas)] -> Bool
verificaRepetidos [] = False
verificaRepetidos (((x,(a,b)):y))
    | l `elem` y = True
    | otherwise = verificaRepetidos y
    where l = (x,(a,b))


-- | Esta função verifica se existe uma porta.
naoexistePorta :: [(Peca, Coordenadas)] -> Bool
naoexistePorta [] = True
naoexistePorta (((x,(a,b)):y)) = if x == Porta then False else naoexistePorta y


-- | Esta função verifica se existe apenas uma porta.
existeApenasUma :: [(Peca, Coordenadas)] -> Bool
existeApenasUma [] = False
existeApenasUma (((x,(a,b)):y)) = if x == Porta then naoexistePorta y else existeApenasUma y


-- | Esta função gera uma lista com as coordenadas de todas as caixas.
posicaoCaixa :: [(Peca, Coordenadas)] -> [Coordenadas]
posicaoCaixa [] = []
posicaoCaixa (((x,(a,b)):y)) = if x == Caixa then (a,b) : posicaoCaixa y else posicaoCaixa y


-- | Esta função verifica se uma caixa ou bloco não estão a flutuar.
verificaBase :: [Coordenadas] -> [(Peca, Coordenadas)] -> Bool
verificaBase ((a,b):y) [] = False
verificaBase [] l = False
verificaBase ((a,b):y) (((z,(c,d)):t))
    | a == c && b == d-1 && (z == Caixa || z == Bloco) || ((verificaBase ((a,b):y) t) || (verificaBase y t)) = True
    | otherwise = False


-- | Esta função verifica se as caixas nao estão a flutuar.
verifica :: [(Peca, Coordenadas)] -> Bool
verifica j@(((x,(a,b)):y)) = if posicaoCaixa j == [] then True else verificaBase (posicaoCaixa j) j


-- | Esta função dá um lista com coordenadas de portas.
posicaoPorta :: [(Peca, Coordenadas)] -> [Coordenadas]
posicaoPorta [] = []
posicaoPorta (((x,(a,b)):y)) = if x == Porta then (a,b) : posicaoPorta y else posicaoPorta y


-- | Esta função verifica se uma porta está a flutuar ou não.
verificaBaseporta :: [Coordenadas] -> [(Peca, Coordenadas)] -> Bool
verificaBaseporta ((a,b):y) [] = False
verificaBaseporta [] l = False
verificaBaseporta ((a,b):y) (((z,(c,d)):t))
    | a == c && b == d-1 && z == Bloco || ((verificaBaseporta ((a,b):y) t) || (verificaBaseporta y t)) = True
    | otherwise = False


-- | Esta função une as duas anteriores para verificar se uma porta está a flutuar.
verificaporta :: [(Peca, Coordenadas)] -> Bool
verificaporta j@(((x,(a,b)):y)) = verificaBaseporta (posicaoPorta j) j



-- | Esta função converte uma lista com peças e coordenadas para apenas coordenadas.
converteCoords :: [(Peca, Coordenadas)] -> [Coordenadas]
converteCoords [] = []
converteCoords (((x,(a,b)):y)) = (a,b) : converteCoords y


-- | Esta função vai calcular a coordenada mais baixa, com y maior.
fundo :: [Coordenadas] -> Coordenadas
fundo [] = (0,0)
fundo [l] = l
fundo ((x,y):(h,t):z) = if y > t then fundo ((x,y):z) else fundo ((h,t):z)


-- | Esta função calcula a coordenada mais à direita, com x maior.
direita :: [Coordenadas] -> Coordenadas
direita [] = (0,0)
direita [l] = l
direita ((x,y):(h,t):z) = if x > h then direita ((x,y):z) else direita ((h,t):z)


-- | Esta função dá a maior coordenada do mapa.
cantodomapa :: Coordenadas -> Coordenadas -> Coordenadas
cantodomapa (a,b) (c,d) = if a > c then (a,d) else (c,b)


-- | Esta função vai gerar todas as coordenadas existentes no mapa.
coordsMapa :: Coordenadas -> [Coordenadas]
coordsMapa (0,0) = [(0,0)]
coordsMapa (x,y)
    | x == 0 && y /= 0 = coordsMapa (0,y-1) ++ [(0,y)]
    | y == 0 && x /= 0 = coordsMapa (x-1,0) ++ [(x,0)]
    | otherwise = coordsMapa (x-1,y) ++ coordsMapa (x,y-1) ++ [(x,y)]


-- | Esta função verifica se uma coordenada pertence a uma lista de coordenadas.
coordnaopertencemapa :: Coordenadas -> [Coordenadas] -> Bool
coordnaopertencemapa _ [] = True
coordnaopertencemapa (x,y) ((a,b):z) = if (x,y) == (a,b) then False else coordnaopertencemapa (x,y) z


-- | Esta função vai verificar se uma lista de coordenadas pertencem ou nao a outra lista de coordenadas.
naopertenceMapa :: [Coordenadas] -> [Coordenadas] -> Bool
naopertenceMapa l [] = True
naopertenceMapa [] l = False
naopertenceMapa ((x,y):a) l = coordnaopertencemapa (x,y) l || naopertenceMapa a l


-- | Esta função vai verificar se existe um espaço vazio.
espacoVazio :: [(Peca, Coordenadas)] -> Bool
espacoVazio [] = True
espacoVazio (((x,(a,b)):y)) = naopertenceMapa (coordsMapa (cantodomapa (direita (converteCoords p)) (fundo (converteCoords p)))) (converteCoords (p))
    where p = ((x,(a,b)):y)




-- | Esta função vai gerar uma lista para cada coluna.
mygroup :: [(Peca,Coordenadas)] -> [[(Peca,Coordenadas)]]
mygroup [] = []
mygroup ((p,(a,b)):t) = ftrue : mygroup ffalse
  where x = mygroup2 ((p,(a,b)):t)
        ftrue = [(p1,(c,d)) | (p1,(c,d)) <- x, c == a]
        ffalse = [(p1,(c,d)) | (p1,(c,d)) <- x, c /= a] 

-- | Esta função vai dar a lista com apenas os blocos.
mygroup2 :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
mygroup2 l = filter (\(x,(a,b)) -> x == Bloco) l


-- | Esta função verifica se uma peça está unida a alguma outra peça do mapa.
blocojunto :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Bool
blocojunto (p,(a,b)) [] = False
blocojunto (p,(a,b)) ((p1,(x,y)):t)
    | b == y && x==a-1 = True
    | otherwise = blocojunto (p,(a,b)) t


-- | Esta função verifica se numa lista há alguma peça unida a uma peça de outra lista.
blocojunto2 :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool 
blocojunto2 _ [] = True
blocojunto2 [] _ = False
blocojunto2 ((p,(a,b)):t) f = if blocojunto ((p,(a,b))) f then True else blocojunto2 t f


-- | Esta função vai comparar se todas as colunas do mapa tem pelo menos uma peça unida a uma peça de outra coluna.
blocojunto3 :: [(Peca,Coordenadas)] -> Int -> Bool 
blocojunto3 l (0) = True
blocojunto3 l x 
    | blocojunto2 ((mygroup (sortOn snd l)) !! (x)) ((mygroup (sortOn snd l)) !! (x-1)) == True = blocojunto3 l (x-1)
    | otherwise = False


-- | Esta função automatiza a blocojunto3.
validaChao :: [(Peca, Coordenadas)] -> Bool
validaChao l = blocojunto3 l (length (mygroup l) -1)



-- | Esta função verifica se uma mapa é verdadeiro.
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = not f1 && f2 && f3 && f4 && f5 && f6
    where f1 = verificaRepetidos pecas
          f2 = existeApenasUma pecas
          f3 = verifica pecas
          f4 = verificaporta pecas
          f5 = espacoVazio pecas
          f6 = validaChao pecas




