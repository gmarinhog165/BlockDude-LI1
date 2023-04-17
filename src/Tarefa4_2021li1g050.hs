{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa4_2021li1g050
Description : Movimentação do personagem
Copyright   : André Miguel Alves de Carvalho <a100818@alunos.uminho.pt>;
            : Gonçalo Duarte Lopes Marinho Gonçalves <a90969@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g050 where

import LI12122
import Tarefa2_2021li1g050
import Tarefa3_2021li1g050 



-- | MOVER ESQUERDA/DIREITA

-- | Esta função define para onde anda no mapa o jogador conforme o movimento atribuido.
mover :: Jogador -> Movimento -> Jogador
mover (Jogador (x,y) d b) AndarEsquerda = Jogador (x-1,y) Oeste b
mover (Jogador (x,y) d b) AndarDireita = Jogador (x+1,y) Este b



-- | Esta função verifica se há algum obstaculo à esquerda.
bloqueioesquerda :: Coordenadas -> [Coordenadas] -> Bool
bloqueioesquerda (x,y) [] = False
bloqueioesquerda (x,y) ((a,b):t) = a==(x-1) && b==y || bloqueioesquerda (x,y) t



-- | Esta função verifica se há algum obstaculo à direita.
bloqueiodireita :: Coordenadas -> [Coordenadas] -> Bool
bloqueiodireita (a,b) [] = False
bloqueiodireita (x,y) ((a,b):t) = a==(x+1) && b==y || bloqueiodireita (x,y) t


-- | Esta função verifica se é possivel andar para a direita.
moverDireita :: Jogador -> Movimento -> Bool -> Jogador
moverDireita (Jogador (x,y) d b) m t = if t then Jogador (x,y) Este b else mover (Jogador (x,y) d b) m

-- | Esta função faz o jogador andar para a direita caso seja possivel. 
moverDireita2 :: Jogador -> Mapa -> Movimento -> Jogador
moverDireita2 (Jogador (x,y) d b) l@(h:t) m = if x == (valorX2 $ desconstroiMapa l) then (Jogador (x,y) Este b)
                                                                                  else moverDireita (Jogador (x,y) d b) m (bloqueiodireita (x,y) ((coordssemPorta l) ))

-- | Esta função verifica se é possivel andar para a esquerda.
moverEsquerda :: Jogador -> Movimento -> Bool -> Jogador
moverEsquerda (Jogador (x,y) d b) m t = if t then Jogador (x,y) Oeste b else mover (Jogador (x,y) d b) m

-- | Esta função faz o jogador andar para a esquerda caso seja possivel.
moverEsquerda2 :: Jogador -> Mapa -> Movimento -> Jogador
moverEsquerda2 (Jogador (x,y) d b) l@(h:t) m = if x == 0 then (Jogador (x,y) Oeste b)
                                                         else moverEsquerda (Jogador (x,y) d b) m (bloqueioesquerda (x,y) (coordssemPorta l))




-- | TREPAR ESQUERDA/DIREITA

-- | Esta função verifica se há uma parede (2y) à direita.
btrepadireita :: Coordenadas -> [Coordenadas] -> Bool
btrepadireita (a,b) [] = False
btrepadireita x ((a,b):t) = x == (a-1,b+1) || btrepadireita x t

-- | Esta função verifica se há uma parede (2y) à esquerda.
btrepaesquerda :: Coordenadas -> [Coordenadas] -> Bool
btrepaesquerda (a,b) [] = False
btrepaesquerda (x,y) ((a,b):t) = x == a+1 && y == b+1  || btrepaesquerda (x,y) t

-- | Esta função dá as condições para o jogador trepar à direita.
trepaDireita :: Jogador -> Movimento -> Bool -> Bool -> Bool -> Jogador
trepaDireita l@(Jogador (x,y) d b) m b1 b2 b3 = if b1 && not b2 && not b3 then (Jogador (x+1,y-1) d b) else l

-- | Esta função dá as condições para o jogador trepar à esquerda.
trepaEsquerda :: Jogador -> Movimento -> Bool -> Bool -> Bool -> Jogador
trepaEsquerda l@(Jogador (x,y) d b) m b1 b2 b3 = if b1 && not b2 && not b3 then (Jogador (x-1,y-1) d b) else l

-- | Esta função aplica as condições para trepar à direita.
trepaDireita2 :: Jogador -> Mapa -> Movimento -> Jogador
trepaDireita2 s@(Jogador (x,y) d b) l m = trepaDireita s m (bloqueiodireita (x,y) (coordssemPorta l)) (btrepadireita (x,y) (coordssemPorta l)) (blocoUP (x,y) (coordssemCaixa l)) 


-- | Esta função aplica as condições para trepar à esquerda.
trepaEsquerda2 :: Jogador -> Mapa -> Movimento -> Jogador
trepaEsquerda2 s@(Jogador (x,y) d b) l m = trepaEsquerda s m (bloqueioesquerda (x,y) (coordssemPorta l)) (btrepaesquerda (x,y) (coordssemPorta l)) (blocoUP (x,y) (coordssemCaixa l))


-- | CAIR INSTANTANEO

-- | Esta função dá a lista de coordenadas duma coluna.
colunaJ :: Coordenadas -> [Coordenadas] -> [Coordenadas]
colunaJ (x,y) [] = []
colunaJ (x,y) ((a,b):t) = if x == a then (a,b) : colunaJ (x,y) t else colunaJ (x,y) t


-- | Esta função remove possiveis blocos por cima de uma coordenada.
upPlayerVazio :: Coordenadas -> [Coordenadas] -> [Coordenadas]
upPlayerVazio (x,y) [] = []
upPlayerVazio (x,y) ((a,b):t) = if y<b then (a,b) : upPlayerVazio (x,y) t else upPlayerVazio (x,y) t


-- | Esta função dá a coordenada do primeiro bloco/caixa por baixo de uma coordenada dada.
primYnacol :: [Coordenadas] -> Coordenadas
primYnacol [(x,y)] = (x,y)
primYnacol ((a,b):(c,d):t) = if b<d then primYnacol ((a,b):t) else primYnacol ((c,d):t)


-- | Esta função verifica se uma coordenada se encontra por cima da outra.
cimaounao :: Coordenadas -> Coordenadas -> Bool
cimaounao (x,y) (a,b) = y+1==b

-- | Esta função faz a caixa cair no mapa quando o jogador a carrega.
quedacomcaixa :: Coordenadas -> Mapa -> Coordenadas
quedacomcaixa (x,y) m = add2y $ primYnacol $ upPlayerVazio (x,y) (colunaJ (x,y) (coordssemPorta m))

-- | Esta função adiciona a uma coordenada 2y.
add2y :: Coordenadas -> Coordenadas
add2y (x,y) = (x,y-2)


-- | MOVER FINAL

-- | Esta função aplica a queda ao movimento do jogador.
movimentofin :: Jogador -> Mapa -> Movimento -> Jogador
movimentofin s@(Jogador (x,y) d b) l m
    | m == AndarDireita = quedajog (moverDireita2 s l m) l
    | m == AndarEsquerda = quedajog (moverEsquerda2 s l m) l

-- | Esta função define as condições para o jogador trepar.
trepafin :: Jogador -> Mapa -> Movimento -> Jogador
trepafin s@(Jogador (x,y) d b) l m = if d == Este && m == Trepar then quedajog (trepaDireita2 s l m) l
                                                                 else if d == Oeste && m == Trepar then quedajog (trepaEsquerda2 s l m) l
                                                                                                   else (Jogador (x,y) d b)



-- | INTERAGIR CAIXAS

-- | Esta função dá as coordenadas das caixas.
coordscaixas :: Mapa -> [Coordenadas]
coordscaixas l = conv $ filter (\(x,(a,b)) -> x == Caixa) $ desconstroiMapa l

-- | Esta função dá as coordenadas dos blocos.
coordsblocos :: Mapa -> [Coordenadas]
coordsblocos l = conv $ filter (\(x,(a,b)) -> x == Bloco) $ desconstroiMapa l


-- | Verifica se há uma caixa à direita.
caixaDireita :: Coordenadas -> [Coordenadas] -> Bool
caixaDireita (x,y) [] = False
caixaDireita (x,y) ((a,b):t) = (x == a-1 && y == b) || caixaDireita (x,y) t

-- | Verifica se há uma caixa à esquerda.
caixaEsquerda :: Coordenadas -> [Coordenadas] -> Bool
caixaEsquerda (x,y) [] = False
caixaEsquerda (x,y) ((a,b):t) = (x == a+1 && y == b) || caixaEsquerda (x,y) t


-- | Verifica se há um um bloco por cima de uma coordenada.
blocoUP :: Coordenadas -> [Coordenadas] -> Bool
blocoUP (x,y) [] = False
blocoUP (x,y) ((a,b):t) = (x == a && b == y-1) || blocoUP (x,y) t

-- | Verifica se há algum bloco por cima da caixa.
blocoUPcaixa :: Coordenadas -> Mapa -> Bool
blocoUPcaixa (x,y) m
    | caixaEsquerda (x,y) (coordscaixas m) && blocoUP (x-1,y) (coordsblocos m) = True
    | caixaDireita (x,y) (coordscaixas m) && blocoUP (x+1,y) (coordsblocos m) = True
    | otherwise = False


-- | Dá as condições para pegar numa caixa à direita.
intCaixaDireita :: Jogador -> Movimento -> Bool -> Bool -> Bool -> Jogador
intCaixaDireita (Jogador (x,y) d b) m b1 b2 b3 = if m == InterageCaixa && not b && b1 && not b2 && not b3 then (Jogador (x,y) d True)
                                                                                                      else (Jogador (x,y) d False)

-- | Dá as condições para pegar numa caixa à esquerda.
intCaixaEsquerda :: Jogador -> Movimento -> Bool -> Bool -> Bool -> Jogador
intCaixaEsquerda (Jogador (x,y) d b) m b1 b2 b3 = if m == InterageCaixa && not b && b1 && not b2 && not b3 then (Jogador (x,y) d True)
                                                                                                       else (Jogador (x,y) d False)

-- | Aplica as condições para pegar numa caixa à direita.
intCaixaDireita2 :: Jogador -> Mapa -> Movimento -> Jogador
intCaixaDireita2 s@(Jogador (x,y) d b) l m = intCaixaDireita s m (caixaDireita (x,y) (coordscaixas l)) (blocoUP (x,y) (conv2 l)) (blocoUPcaixa (x,y) l)

-- | Aplica as condições para pegar numa caixa à direita.
intCaixaEsquerda2 :: Jogador -> Mapa -> Movimento -> Jogador
intCaixaEsquerda2 s@(Jogador (x,y) d b) l m = intCaixaEsquerda s m (caixaEsquerda (x,y) (coordscaixas l)) (blocoUP (x,y) (conv2 l)) (blocoUPcaixa (x,y) l)

-- | Junta e dá as condições para pegar numa caixa à direita ou esquerda.
intCaixaGeral :: Jogador -> Mapa -> Movimento -> Jogador
intCaixaGeral (Jogador (x,y) d b) l m = if m == InterageCaixa && d == Este then intCaixaDireita2 (Jogador (x,y) d b) l m
                                                                         else if m == InterageCaixa && d == Oeste then intCaixaEsquerda2 (Jogador (x,y) d b) l m
                                                                                                                  else (Jogador (x,y) d b)

-- | Esta função dá as condições para dar drop à caixa assim como as suas alterações no jogador.
dropCaixa :: Jogador -> Movimento -> Bool -> Jogador
dropCaixa (Jogador (x,y) d b) m b1 = if b && not b1 then (Jogador (x,y) d False)
                                                    else (Jogador (x,y) d True)

-- | Esta função verifica se é possivel dar drop da caixa à direita.
dropCaixaDireita :: Jogador -> Mapa -> Movimento -> Jogador
dropCaixaDireita s@(Jogador (x,y) d b) l m = dropCaixa s m (bloqueiodireita (x,y-1) (conv2 l))


-- | Esta função verifica se é possivel dar drop da caixa à esquerda.
dropCaixaEsquerda :: Jogador -> Mapa -> Movimento -> Jogador
dropCaixaEsquerda s@(Jogador (x,y) d b) l m = dropCaixa s m (bloqueioesquerda (x,y-1) (conv2 l))


-- | Função que reune as condições para dar drop da caixa à esquerda ou direita.
dropCaixaGeral :: Jogador -> Mapa -> Movimento -> Jogador
dropCaixaGeral (Jogador (x,y) d b) l m = if m == InterageCaixa && d == Este then dropCaixaDireita (Jogador (x,y) d b) l m
                                                                             else if m == InterageCaixa && d == Oeste then dropCaixaEsquerda (Jogador (x,y) d b) l m
                                                                                                                      else (Jogador (x,y) d b)


-- | Função que dá as condições para pegar ou dar drop numa caixa.
intDropCaixa :: Jogador -> Mapa -> Movimento -> Jogador
intDropCaixa s@(Jogador (x,y) d b) l m = if not b && m == InterageCaixa then intCaixaGeral s l m
                                                                        else if b && m == InterageCaixa then dropCaixaGeral s l m
                                                                                                        else s


-- | ALTERAÇÃO DO MAPA


-- | Esta função altera a peça numa linha do mapa.
altmapa :: Coordenadas -> (Peca, Coordenadas) -> [Peca] -> [Peca]
altmapa (x,y) (p,(a,b)) [] = []
altmapa (x,y) (p,(a,b)) (h:t) = if x == a && y == b then (p:t) else h : altmapa (x+1,y) (p,(a,b)) t


-- | Esta função altera a peça mas devolve o mapa em vez de só a linha.
altmapa2 :: Coordenadas -> (Peca, Coordenadas) -> Mapa -> Mapa
altmapa2 (x,y) (p,(a,b)) [] = []
altmapa2 (x,y) (p,(a,b)) ((h:t):xs) = altmapa (x,y) (p,(a,b)) (h:t) : altmapa2 (x,y+1) (p,(a,b)) xs


-- | Esta função altera as peças num mapa e devolve o mapa.
altmapageral :: [(Peca, Coordenadas)] -> Mapa -> Mapa
altmapageral t m = foldl (flip (altmapa2 (0, 0))) m t


-- | Esta função altera o mapa consoante o movimento do jogador.
altmovmapa :: String -> Jogador -> Mapa -> Mapa
altmovmapa s j@(Jogador (x,y) d b) m
    | s == "Drop" && d == Oeste && not (blocoesqjog j m) && not (blocoesqcaixa j m)  = altmapageral [(Vazio,(x,y-1)), (Caixa, novaposcaixaesq j m)] m
    | s == "Drop" && d == Oeste && blocoesqjog j m && not (blocoesqcaixa j m)  = altmapageral [(Vazio,(x,y-1)),(Caixa,add1y $ novaposcaixaesq j m)] m
    | s == "Drop" && d == Este && not (blocodirjog j m) && not (blocodircaixa j m) = altmapageral [(Vazio,(x,y-1)),(Caixa, novaposcaixadir j m)] m
    | s == "Drop" && d == Este && blocodirjog j m && not (blocodircaixa j m) = altmapageral [(Vazio,(x,y-1)),(Caixa,add1y $ add1y $ novaposcaixadir j m)] m
    | s == "Interage" && bloqueioesquerda (x,y) (coordscaixas m) && d == Oeste = altmapageral [(Vazio,(x-1,y)),(Caixa,(x,y-1))] m
    | s == "Interage" && bloqueiodireita (x,y) (coordscaixas m) && d == Este = altmapageral [(Vazio,(x+1,y)),(Caixa,(x,y-1))] m
    | s == "Still" && b = altmapageral [(Caixa,(x,y-1))] m
    | otherwise = m


-- | Esta função dá a nova posição da caixa quando o jogador lhe dá drop para a esquerda.
novaposcaixaesq :: Jogador -> Mapa -> Coordenadas 
novaposcaixaesq (Jogador (x,y) d b) m = add1y $ primYnacol $ upPlayerVazio (x,y) (colunaJ (x-1,y) (conv $ desconstroiMapa m))

-- | Esta função dá a nova posição da caixa quando o jogador lhe dá drop para a direita.
novaposcaixadir :: Jogador -> Mapa -> Coordenadas 
novaposcaixadir (Jogador (x,y) d b) m = add1y $ primYnacol $ upPlayerVazio (x,y) (colunaJ (x+1,y) (conv $ desconstroiMapa m))

-- | Esta função verifica se há um bloqueio à esquerda.
blocoesqjog :: Jogador -> Mapa -> Bool 
blocoesqjog (Jogador (x,y) d b) m = (bloqueioesquerda (x,y) (conv2 m))

-- | Esta função verifica se há um bloqueio à direita.
blocodirjog :: Jogador -> Mapa -> Bool 
blocodirjog (Jogador (x,y) d b) m = (bloqueiodireita (x,y) (conv2 m))

-- | Esta função verifica se há um bloqueio à esquerda da caixa.
blocoesqcaixa :: Jogador -> Mapa -> Bool 
blocoesqcaixa (Jogador (x,y) d b) m = (bloqueioesquerda (x,y-1) (conv2 m))

-- | Esta função verifica se há um bloqueio à direita da caixa.
blocodircaixa :: Jogador -> Mapa -> Bool 
blocodircaixa (Jogador (x,y) d b) m = (bloqueiodireita (x,y-1) (conv2 m))

-- | Esta função adiciona uma unidade ao y de coordenadas.
add1y :: Coordenadas -> Coordenadas
add1y (x,y) = (x,y-1)


-- | Esta função atualiza um jogo conforme um movimento.
altmovmapageral :: Jogo -> Movimento -> Jogo
altmovmapageral j@(Jogo l (Jogador (x,y) d b)) m = if m == InterageCaixa then (Jogo (altmovmapa (intDrop b (h $ intDropCaixa (Jogador (x,y) d b) l m)) (Jogador (x,y) d b) l) (intDropCaixa (Jogador (x,y) d b) l m)) else j
   where
       h (Jogador (x,y) d b) = b


-- | Esta função dá condições e atribui nomes consoante a veracidade de cada bool.
intDrop :: Bool -> Bool -> String
intDrop x y = if x && not y then "Drop" else if not x && y then "Interage" else "Still"


-- | Esta função altera um jogo conforme o seu movimento ao carregar uma caixa consigo.
movJogCaixa :: Jogo -> Movimento -> Jogo
movJogCaixa (Jogo l (Jogador (x,y) d b)) m
    | b && m == AndarDireita && bloqueiodireita (x,y) (conv2 l) = Jogo (altmapageral [(Caixa,(x,y-1))] l) (Jogador (x,y) Este True)
    | b && m == AndarDireita && bloqueiodireita (x,y-1) (conv2 l)= Jogo (altmapageral [(Caixa,(x,y-1))] l) (Jogador (x,y) Este True)
    | b && m == AndarEsquerda && bloqueioesquerda (x,y) (conv2 l) = Jogo (altmapageral [(Caixa,(x,y-1))] l) (Jogador (x,y) Oeste b)
    | b && m == AndarEsquerda && bloqueioesquerda (x,y-1) (conv2 l) = Jogo (altmapageral [(Caixa,(x,y-1))] l) (Jogador (x,y) Oeste b)
    | b && m == AndarDireita = Jogo (altmapageral [(Vazio,(x,y-1)),(Caixa,quedacomcaixa (x+1,y) l)] l) (movimentofin (Jogador (x,y) d b) l m)
    | b && m == AndarEsquerda = Jogo (altmapageral [(Vazio,(x,y-1)),(Caixa,quedacomcaixa (x-1,y) l)] l) (movimentofin (Jogador (x,y) d b) l m)
    | otherwise = Jogo l (movimentofin (Jogador (x,y) d b) l m)


-- | Esta função altera um jogo quando um jogador trepa enquanto carrega uma caixa.
trepacCaixa :: Jogo -> Movimento -> Jogo
trepacCaixa (Jogo l (Jogador (x,y) d b)) m
    | b && m == Trepar && d == Este && (bloqueiodireita (x,y) (coordssemPorta l)) && not (btrepadireita (x,y) (coordssemPorta l)) = (Jogo (altmapageral [(Vazio,(x,y-1)),(Caixa,(x+1,y-2))] l) (trepafin (Jogador (x,y) d b) l m))
    | b && m == Trepar && d == Oeste && (bloqueioesquerda (x,y) (coordssemPorta l)) && not (btrepaesquerda (x,y) (coordssemPorta l)) = (Jogo (altmapageral [(Vazio,(x,y-1)),(Caixa,(x-1,y-2))] l) (trepafin (Jogador (x,y) d b) l m))
    | x == 1 = (Jogo (altmapageral [(Caixa,(x,y-1))] l) (Jogador (x,y) d b))
    | otherwise = (Jogo (altmapageral [(Caixa,(x,y-1))] l) (Jogador (x,y) d b))


-- | FUNÇOES FINAIS 

-- | Esta função altera o jogo conforme um movimento dum jogador.
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo l (Jogador (x,y) d b)) m
    | not b && m == AndarDireita || not b && m == AndarEsquerda = Jogo l (movimentofin (Jogador (x,y) d b) l m)
    | not b && m == Trepar = Jogo l (trepafin (Jogador (x,y) d b) l m)
    | not b && m == InterageCaixa = altmovmapageral (Jogo l (Jogador (x,y) d b)) m
    | b && m == InterageCaixa = altmovmapageral (Jogo l (Jogador (x,y) d b)) m
    | b && m == AndarEsquerda || b && m == AndarDireita = movJogCaixa (Jogo l (Jogador (x,y) d b)) m
    | b && m == Trepar = trepacCaixa (Jogo l (Jogador (x,y) d b)) m
    | otherwise = Jogo l (Jogador (x,y) d b)


-- | Esta função altera um jogo conforme uma lista de movimentos atribuidos a um jogador.
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo l (Jogador (x,y) d b)) [] = (Jogo l (Jogador (x,y) d b))
correrMovimentos t@(Jogo l (Jogador (x,y) d b)) (m:ms) = correrMovimentos (moveJogador t m) ms

j2 :: Jogo
j2 = Jogo mapa2 (Jogador (8,5) Oeste False)

mapa2 :: Mapa
mapa2 = [
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]