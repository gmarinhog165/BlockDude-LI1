{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- |
Module      : Tarefa5_2021li1g050
Description : Aplicação Gráfica
Copyright   : André Miguel Alves de Carvalho <a100818@alunos.uminho.pt>;
            : Gonçalo Duarte Lopes Marinho Gonçalves <a90969@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa2_2021li1g050
import Graphics.Rendering.OpenGL (debugMessageInsert)
import Graphics.Gloss.Juicy (loadJuicy)
import LI12122
import Data.Maybe
import Codec.Picture
import Tarefa3_2021li1g050
import Tarefa4_2021li1g050


data EstadoGloss = Estado {
                  textures ::Texturas,
                  game :: Jogo,
                  menu :: Menu }

type Texturas =  [(Texture, Picture)]

data Texture = Void | Box | Door | Block | Player | Menu deriving Eq

newtype Menu = Main Optn

data Optn = Principal | Game | Levels | Solution

{- Esta funçao dá o estado Inicial do Gloss -}

estadoGlossInicial :: Menu -> Texturas -> Jogo -> EstadoGloss
estadoGlossInicial m (h:t) j  = Estado (h:t) j m

 {- Esta função desenha uma linha do Mapa -}

desenhaLinhaMapa :: Float -> Float -> [Peca] -> Texturas -> [Picture]
desenhaLinhaMapa x y (h:t) m
                          | h == Vazio = desenhaLinhaMapa (x+200) y t m
                          | h == Bloco = translate x y (fromJust (lookup Block m)) : desenhaLinhaMapa (x+200) y t m
                          | h == Caixa = translate x y (fromJust (lookup Box m)) : desenhaLinhaMapa (x+200) y t m
                          | h == Porta = translate x y (fromJust (lookup Door m)) : desenhaLinhaMapa (x+200) y t m
desenhaLinhaMapa _ _ _ _ = []

{- Esta função desenha o mapa na sua totalidade-}

desenhaMapa :: Float ->Float -> Mapa -> Texturas -> [Picture]
desenhaMapa x y (h:t) m = desenhaLinhaMapa x y h m ++ desenhaMapa x (y-200) t m
desenhaMapa _ _ [] _ = []

 {- Esta Função desenha o jogador -}
desenhaJogador :: Float -> Float ->  Jogador -> Texturas -> [Picture]
desenhaJogador x y (Jogador (a,b) c l) t
                                       | fromIntegral a == x && fromIntegral b == y = [translate x y (fromJust (lookup Player t))]
                                       | fromIntegral a /= x && fromIntegral b == y = desenhaJogador (x+200) y (Jogador (a,b) c l) t
                                       | fromIntegral a == x && fromIntegral b /= y = desenhaJogador x (y+200) (Jogador (a,b) c l) t
                                       | fromIntegral a /= x && fromIntegral b /= y = desenhaJogador (x+200) (y+200) (Jogador (a,b) c l) t

{- Esta Função reage às teclas pressioadas -}

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeyF1) Down _ _ ) (Estado t j (Main Principal) ) = Estado t j (Main Game)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (Estado t j (Main Game)) = Estado t (moveJogador j AndarEsquerda) (Main Game)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (Estado t j (Main Game)) = Estado t (moveJogador j AndarDireita) (Main Game)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Estado t j (Main Game)) = Estado t (moveJogador j Trepar) (Main Game)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Estado t j (Main Game)) = Estado t (moveJogador j InterageCaixa) (Main Game)
reageEventoGloss _  e = e

{- Esta função reage ao passar do tempo do gloss -}

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss n (Estado t j p) =  Estado t j p

{- Esta função da a frame rate do jogo -}

fr :: Int
fr = 50

{- Esta função define o tamanho do display do jogo -}

screen :: Display
screen = FullScreen

{- Esta função desenha os aacontecimentos do gloss-}

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (Estado ((m,n):t) j (Main Principal))
                                                    | m == Menu = n
                                                    | otherwise = desenhaEstadoGloss (Estado t j (Main Principal))
desenhaEstadoGloss (Estado t (Jogo m j) (Main Game)) = pictures (desenhaMapa (-600) 100 m t ++ desenhaJogador 300 50 j t)

main :: IO ()
main = do
     Just menu <- loadJuicy "2021li1g050/Sprites/Menu.png"
     Just box <- loadJuicy "2021li1g050/Sprites/Caixa.png"
     Just door <- loadJuicy "2021li1g050/Sprites/Porta.png"
     Just block <- loadJuicy "2021li1g050/Sprites/Bloco.png"
     Just player <- loadJuicy "2021li1g050/Sprites/Player.png"
     play screen
       black
       fr
       (estadoGlossInicial (Main Principal) [
        (Box, box),
        (Block, block),
        (Door, door),
        (Player, player),
        (Menu, menu)] (Jogo (escolheMapa 1) (escolheJogador 1)))
      desenhaEstadoGloss
      reageEventoGloss
      reageTempoGloss






