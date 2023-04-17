module Tarefa4_2021li1g050_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g050
import Tarefa4_2021li1g050
import Fixtures

testsT4 =
  test
    [ "Movimento Trepar no Jogo 01" ~: jogo01 ~=? moveJogador jogo01 Trepar
    , "Movimento Trepar no Jogo 02" ~: Jogo mapa01 (Jogador (4, 2) Oeste True) ~=? moveJogador jogo02 Trepar
    , "Movimento Trepar Porta" ~: Jogo mapa06 (Jogador (1, 3) Oeste False) ~=? moveJogador (Jogo mapa06 (Jogador (1, 3) Oeste False)) Trepar
    , "Movimento Trepar para cima bloco" ~: Jogo mapa05 (Jogador (2, 2) Este False) ~=? moveJogador (Jogo mapa05 (Jogador (1, 3) Este False)) Trepar
    , "Movimento Trepar para Este contra parede" ~: Jogo mapa01 (Jogador (5, 3) Este False) ~=? moveJogador (Jogo mapa01 (Jogador (5, 3) Este False)) Trepar
    , "Movimento Trepar para Este com caixa" ~: Jogo mapa01 (Jogador (4, 2) Este True) ~=? moveJogador (Jogo mapa01 (Jogador (4, 2) Este True)) Trepar
    , "Movimento Trepar para Oeste para cima de Caixa" ~: Jogo mapa01 (Jogador (4, 2) Oeste False) ~=? moveJogador (Jogo mapa01 (Jogador (5, 3) Oeste False)) Trepar
    , "Movimento Trepar para Este para cima de Caixa" ~: Jogo mapa01 (Jogador (4, 2) Este False) ~=? moveJogador (Jogo mapa01 (Jogador (3, 3) Este False)) Trepar
    , "Movimento Trepar para Oeste para cima de Caixa com Caixa" ~: Jogo mapa01 (Jogador (4, 2) Oeste True) ~=? moveJogador (Jogo mapa01 (Jogador (5, 3) Oeste True)) Trepar
    , "Movimento Trepar para Este para cima de Caixa com Caixa" ~: Jogo mapa01 (Jogador (4, 2) Este True) ~=? moveJogador (Jogo mapa01 (Jogador (3, 3) Este True)) Trepar
    , "Movimento Trepar para Oeste com Caixa bloqueda por bloco" ~: Jogo mapa05 (Jogador (3, 3) Oeste True) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Oeste True)) Trepar
    , "Movimento Trepar para Oeste para entre blocos" ~: Jogo mapa05 (Jogador (2, 2) Oeste False) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Oeste False)) Trepar
    , "Movimento AndarEsquerda bloqueado pela caixa contra bloco" ~: Jogo mapa07 (Jogador (4, 1) Oeste True) ~=? moveJogador (Jogo mapa07 (Jogador (4, 1) Oeste True)) AndarEsquerda
    , "Movimento AndarEsquerda no Jogo 03" ~: Jogo mapa09 (Jogador (16, 6) Oeste False) ~=? moveJogador jogo03 AndarEsquerda
    , "Movimento AndarEsquerda para cima caixa" ~: Jogo mapa09 (Jogador (11, 6) Oeste False) ~=? moveJogador (Jogo mapa09 (Jogador (12, 4) Este False)) AndarEsquerda
    , "Movimento AndarEsquerda para cima caixa com caixa" ~: Jogo mapa09 (Jogador (11, 6) Oeste True) ~=? moveJogador (Jogo mapa09 (Jogador (12, 4) Este True)) AndarEsquerda
    , "Movimento AndarEsquerda de um muro" ~: Jogo mapa01 (Jogador (5,3) Oeste False) ~=? moveJogador jogo01 AndarEsquerda
    , "Movimento AndarDireita no Jogo 03" ~: Jogo mapa09 (Jogador (18, 6) Este False) ~=? moveJogador jogo03 AndarDireita
    , "Movimento AndarDireita no Jogo 04" ~: Jogo mapa10 (Jogador (13, 6) Este False) ~=? moveJogador jogo04 AndarDireita
    , "Movimento AndarDireita no Jogo 06" ~: Jogo mapa06 (Jogador (3, 2) Este False) ~=? moveJogador jogo06 AndarDireita
    , "Movimento AndarDireita descer de Caixa com Caixa" ~: Jogo mapa06 (Jogador (4, 3) Este True) ~=? moveJogador (Jogo mapa06 (Jogador (3, 2) Este True)) AndarDireita
    , "Movimento AndarDireita contra bloco" ~: Jogo mapa05 (Jogador (1, 3) Este False) ~=? moveJogador (Jogo mapa05 (Jogador (1, 3) Este False)) AndarDireita
    , "Movimento AndarDireita de um muro" ~: Jogo mapa10 (Jogador (13,6) Este False) ~=? moveJogador jogo04 AndarDireita
    , "Movimento larga Caixa" ~: Jogo (insertAt (3,3) Caixa mapa04) (Jogador (4, 3) Oeste False) ~=? moveJogador (Jogo mapa04 (Jogador (4, 3) Oeste True)) InterageCaixa
    , "Movimento tenta largar caixa Oeste" ~: Jogo mapa04 (Jogador (3, 3) Oeste True) ~=? moveJogador (Jogo mapa04 (Jogador (3, 3) Oeste True)) InterageCaixa
    , "Movimento tenta pegar Caixa Este" ~: Jogo mapa04 (Jogador (4, 3) Oeste False) ~=? moveJogador (Jogo mapa04 (Jogador (4, 3) Oeste False)) InterageCaixa
    , "Movimento levanta Caixa" ~: Jogo mapa04 (Jogador (4, 3) Oeste False) ~=? moveJogador (Jogo mapa04 (Jogador (4, 3) Oeste False)) InterageCaixa
    , "Movimento levanta Caixa" ~: Jogo (insertAt (2,1) Vazio mapa07) (Jogador (1, 1) Este True) ~=?  moveJogador (Jogo mapa07 (Jogador (1, 1) Este False)) InterageCaixa
    , "Movimento tenta levantar Caixa" ~: Jogo mapa07 (Jogador (4, 1) Este False) ~=?  moveJogador (Jogo mapa07 (Jogador (4, 1) Este False)) InterageCaixa
    , "Movimento larga Caixa entre blocos" ~: Jogo (insertAt (2, 2) Caixa mapa05) (Jogador (3, 3) Oeste False) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Oeste True)) InterageCaixa
    , "Movimento larga Caixa" ~: Jogo (insertAt (4, 3) Caixa mapa05) (Jogador (3, 3) Este False) ~=? moveJogador (Jogo mapa05 (Jogador (3, 3) Este True)) InterageCaixa
    , "Movimento tenta largar Caixa" ~: Jogo mapa05 (Jogador (5, 3) Este True) ~=? moveJogador (Jogo mapa05 (Jogador (5, 3) Este True)) InterageCaixa
    , "Movimento tenta largar Caixa em cima de Porta Oeste" ~: Jogo mapa01 (Jogador (1,3) Oeste True) ~=? moveJogador (Jogo mapa01 (Jogador (1,3) Oeste True)) InterageCaixa
    , "Movimento tenta largar Caixa em cima de Porta Este" ~: Jogo mapa10 (Jogador (0,6) Este True) ~=?  moveJogador (Jogo mapa10 (Jogador (0,6) Este True)) InterageCaixa
    , "Movimento largar Caixa para baixo de Porta" ~: Jogo (insertAt (1,3) Caixa mapa14) (Jogador (2,1) Oeste False) ~=?  moveJogador (Jogo mapa14 (Jogador (2,1) Oeste True)) InterageCaixa
    , "Movimento tenta levantar Caixa bloqueada por bloco" ~: Jogo mapa08 (Jogador (3, 1) Oeste False) ~=? moveJogador (Jogo mapa08 (Jogador (3, 1) Oeste False)) InterageCaixa
    , "Movimento levanta Caixa" ~: Jogo (insertAt (4,3) Vazio mapa12) (Jogador (5, 3) Oeste True) ~=? moveJogador (Jogo mapa12 (Jogador (5, 3) Oeste False)) InterageCaixa
    , "Resolve Jogo 01" ~: Jogo mapa01 (Jogador (0, 3) Oeste False) ~=? correrMovimentos jogo01 [AndarEsquerda, Trepar, AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Resolve Jogo 07" ~: Jogo (insertAt (0,3) Caixa mapa12) (Jogador (0, 2) Oeste False) ~=? correrMovimentos jogo07 [Trepar, AndarEsquerda, Trepar, AndarEsquerda, InterageCaixa, Trepar]
    , "Resolve Jogo 03" ~: Jogo (foldr (uncurry insertAt) mapa09 [((10, 6), Vazio), ((15 ,6), Vazio), ((5, 6), Caixa), ((13, 6), Caixa)]) (Jogador (1, 6) Oeste False) ~=?  correrMovimentos jogo03 [AndarEsquerda, InterageCaixa, AndarEsquerda, AndarEsquerda, InterageCaixa, Trepar, Trepar, AndarEsquerda, InterageCaixa, AndarEsquerda, AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda, InterageCaixa, Trepar, Trepar, AndarEsquerda, AndarEsquerda, AndarEsquerda]
    , "Passear no Jogo 09" ~: Jogo mapa14 (Jogador (0,3) Oeste True) ~=? correrMovimentos jogo09 [AndarEsquerda, InterageCaixa, InterageCaixa, AndarEsquerda ]
    ]