module Fixtures where

import LI12122

insertAt :: (Int, Int) -> a -> [[a]] -> [[a]]
insertAt (x, y) item grelha = take y grelha ++ linha : drop (y + 1) grelha
  where
    lista = grelha !! y
    linha = take x lista ++ item : drop (x + 1) lista

invalido01 :: [(Peca, Coordenadas)]
invalido01 = [ (Bloco, (4, 5)), (Bloco, (12, 5)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

invalido02 :: [(Peca, Coordenadas)]
invalido02 = [ (Bloco, (4, 5)), (Bloco, (12, 5)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

invalido03 :: [(Peca, Coordenadas)]
invalido03 = [ (Bloco, (0, 0)), (Bloco, (19, 0)), (Bloco, (0, 1)), (Bloco, (19, 1)), (Bloco, (0, 2)), (Bloco, (19, 2)), (Bloco, (0, 3)), (Bloco, (19, 3)), (Bloco, (0, 4)), (Bloco, (19, 4)), (Bloco, (0, 5)), (Bloco, (4, 5)), (Caixa, (12, 4)), (Bloco, (19, 5)), (Bloco, (0, 6)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (19, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (15, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

invalido04 :: [(Peca, Coordenadas)]
invalido04 = [ (Bloco, (19, 0)), (Bloco, (18, 1)), (Bloco, (20, 1)), (Bloco, (17, 2)), (Bloco, (21, 2)), (Bloco, (7, 3)), (Bloco, (16, 3)), (Bloco, (22, 3)), (Bloco, (6, 4)), (Bloco, (8, 4)), (Bloco, (15, 4)), (Bloco, (23, 4)), (Bloco, (3, 5)), (Bloco, (4, 5)), (Bloco, (5, 5)), (Bloco, (9, 5)), (Bloco, (14, 5)), (Bloco, (24, 5)), (Bloco, (2, 6)), (Bloco, (10, 6)), (Bloco, (13, 6)), (Bloco, (24, 6)), (Bloco, (1, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (24, 7)), (Bloco, (1, 8)), (Bloco, (24, 8)), (Bloco, (1, 9)), (Caixa, (23, 9)), (Bloco, (24, 9)), (Bloco, (1, 10)), (Caixa, (22, 10)), (Caixa, (23, 10)), (Bloco, (24, 10)), (Bloco, (1, 11)), (Bloco, (22, 11)), (Bloco, (23, 11)), (Bloco, (24, 11)), (Bloco, (0, 12)), (Bloco, (1, 12)), (Bloco, (6, 12)), (Bloco, (17, 12)), (Bloco, (0, 13)), (Porta, (1, 13)), (Bloco, (6, 13)), (Caixa, (8, 13)), (Bloco, (17, 13)), (Bloco, (18, 13)), (Bloco, (19, 13)), (Bloco, (20, 13)), (Bloco, (21, 13)), (Bloco, (22, 13)), (Bloco, (0, 14)), (Bloco, (1, 14)), (Bloco, (2, 14)), (Bloco, (3, 14)), (Bloco, (4, 14)), (Bloco, (6, 14)), (Caixa, (8, 14)), (Caixa, (12, 14)), (Bloco, (15, 14)), (Bloco, (16, 14)), (Bloco, (17, 14)), (Bloco, (4, 15)), (Bloco, (6, 15)), (Caixa, (8, 15)), (Bloco, (10, 15)), (Bloco, (12, 15)), (Caixa, (13, 15)), (Bloco, (15, 15)), (Bloco, (4, 16)), (Bloco, (6, 16)), (Bloco, (7, 16)), (Bloco, (8, 16)), (Bloco, (9, 16)), (Bloco, (10, 16)), (Bloco, (11, 16)), (Bloco, (12, 16)), (Bloco, (13, 16)), (Bloco, (14, 16)), (Bloco, (15, 16)), (Bloco, (4, 17)), (Bloco, (5, 17)), (Bloco, (6, 17)) ]

invalido05 :: [(Peca, Coordenadas)]
invalido05 = [ (Porta, (0, 0)) , (Bloco, (0, 1)) , (Bloco, (1, 1)) , (Porta, (4, 0)) , (Bloco, (2, 1)) , (Bloco, (3, 1)) , (Bloco, (4, 1)) , (Bloco, (5, 1)) ]

invalido06 :: [(Peca, Coordenadas)]
invalido06 = [(Bloco, (0, 1)), (Bloco, (1, 1)), (Bloco, (2, 1)), (Bloco, (3, 1)), (Bloco, (4, 1)), (Bloco, (5, 1))]

invalido07 :: [(Peca, Coordenadas)]
invalido07 = [(Bloco, (5, 0)), (Bloco, (4, 0)), (Bloco, (3, 0)), (Bloco, (2, 0)), (Porta, (1, 0)), (Bloco, (0, 0)), (Bloco, (0, 1)), (Bloco, (1, 1)), (Bloco, (2, 1)), (Bloco, (3, 1)), (Bloco, (4, 1)), (Bloco, (5, 1))]

invalido08 :: [(Peca, Coordenadas)]
invalido08 = [(Bloco, (6, 1)), (Bloco, (2, 2)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Bloco, (6, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4))]

lista01 :: [(Peca, Coordenadas)]
lista01 = [(Porta, (0, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (2, 3)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Caixa, (4, 3)), (Bloco, (5, 4)), (Bloco, (6, 4)), (Bloco, (6, 3)), (Bloco, (6, 2)), (Bloco, (6, 1)) ]

mapa01 :: Mapa
mapa01 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista02 :: [(Peca, Coordenadas)]
lista02 = [ (Porta, (0, 0)), (Bloco, (0, 1)), (Bloco, (1, 1)), (Bloco, (2, 1)), (Bloco, (3, 1)), (Bloco, (4, 1)), (Bloco, (5, 1)) ]

mapa02 :: Mapa
mapa02 =
  [ [Porta, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista03 :: [(Peca, Coordenadas)]
lista03 = [ (Porta, (0, 1)), (Bloco, (2, 1)), (Bloco, (0, 2)), (Bloco, (1, 2)), (Bloco, (2, 2)), (Bloco, (3, 2)), (Bloco, (4, 2)), (Bloco, (5, 2)) ]

mapa03 :: Mapa
mapa03 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista04 :: [(Peca, Coordenadas)]
lista04 = [ (Bloco, (6, 1)), (Bloco, (2, 2)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4)) ]

mapa04 :: Mapa
mapa04 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista05 :: [(Peca, Coordenadas)]
lista05 = [ (Bloco, (2, 1)), (Bloco, (6, 1)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4)) ]

mapa05 :: Mapa
mapa05 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista06 :: [(Peca, Coordenadas)]
lista06 = [ (Bloco, (6, 1)), (Bloco, (2, 2)), (Bloco, (6, 2)), (Porta, (0, 3)), (Bloco, (2, 3)), (Caixa, (3, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4)) ]

mapa06 :: Mapa
mapa06 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista07 :: [(Peca, Coordenadas)]
lista07 = [ (Bloco, (3, 0)), (Porta, (0, 1)), (Caixa, (2, 1)), (Bloco, (0, 2)), (Bloco, (1, 2)), (Bloco, (2, 2)), (Bloco, (3, 2)), (Bloco, (4, 2)), (Bloco, (5, 2)) ]

mapa07 :: Mapa
mapa07 =
  [ [Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Porta, Vazio, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista08 :: [(Peca, Coordenadas)]
lista08 = [ (Bloco, (2, 0)), (Porta, (0, 1)), (Caixa, (2, 1)), (Bloco, (0, 2)), (Bloco, (1, 2)), (Bloco, (2, 2)), (Bloco, (3, 2)), (Bloco, (4, 2)), (Bloco, (5, 2)) ]

mapa08 :: Mapa
mapa08 =
  [ [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Porta, Vazio, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista09 :: [(Peca, Coordenadas)]
lista09 = [ (Bloco, (0, 0)), (Bloco, (19, 0)), (Bloco, (0, 1)), (Bloco, (19, 1)), (Bloco, (0, 2)), (Bloco, (19, 2)), (Bloco, (0, 3)), (Bloco, (19, 3)), (Bloco, (0, 4)), (Bloco, (19, 4)), (Bloco, (0, 5)), (Bloco, (4, 5)), (Bloco, (12, 5)), (Bloco, (19, 5)), (Bloco, (0, 6)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (19, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (15, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

mapa09 :: Mapa
mapa09 =
  [ [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista10 :: [(Peca, Coordenadas)]
lista10 = [ (Bloco, (4, 5)), (Bloco, (12, 5)), (Porta, (1, 6)), (Bloco, (4, 6)), (Bloco, (8, 6)), (Caixa, (10, 6)), (Bloco, (12, 6)), (Caixa, (15, 6)), (Bloco, (0, 7)), (Bloco, (1, 7)), (Bloco, (2, 7)), (Bloco, (3, 7)), (Bloco, (4, 7)), (Bloco, (5, 7)), (Bloco, (6, 7)), (Bloco, (7, 7)), (Bloco, (8, 7)), (Bloco, (9, 7)), (Bloco, (10, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (13, 7)), (Bloco, (14, 7)), (Bloco, (15, 7)), (Bloco, (16, 7)), (Bloco, (17, 7)), (Bloco, (18, 7)), (Bloco, (19, 7)) ]

mapa10 :: Mapa
mapa10 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista11 :: [(Peca, Coordenadas)]
lista11 = [ (Bloco, (19, 0)), (Bloco, (18, 1)), (Bloco, (20, 1)), (Bloco, (17, 2)), (Bloco, (21, 2)), (Bloco, (7, 3)), (Bloco, (16, 3)), (Bloco, (22, 3)), (Bloco, (6, 4)), (Bloco, (8, 4)), (Bloco, (15, 4)), (Bloco, (23, 4)), (Bloco, (3, 5)), (Bloco, (4, 5)), (Bloco, (5, 5)), (Bloco, (9, 5)), (Bloco, (14, 5)), (Bloco, (24, 5)), (Bloco, (2, 6)), (Bloco, (10, 6)), (Bloco, (13, 6)), (Bloco, (24, 6)), (Bloco, (1, 7)), (Bloco, (11, 7)), (Bloco, (12, 7)), (Bloco, (24, 7)), (Bloco, (1, 8)), (Bloco, (24, 8)), (Bloco, (1, 9)), (Caixa, (23, 9)), (Bloco, (24, 9)), (Bloco, (1, 10)), (Caixa, (22, 10)), (Caixa, (23, 10)), (Bloco, (24, 10)), (Bloco, (1, 11)), (Bloco, (22, 11)), (Bloco, (23, 11)), (Bloco, (24, 11)), (Bloco, (0, 12)), (Bloco, (1, 12)), (Bloco, (6, 12)), (Bloco, (17, 12)), (Bloco, (22, 12)), (Bloco, (0, 13)), (Porta, (1, 13)), (Bloco, (6, 13)), (Caixa, (8, 13)), (Bloco, (17, 13)), (Bloco, (18, 13)), (Bloco, (19, 13)), (Bloco, (20, 13)), (Bloco, (21, 13)), (Bloco, (22, 13)), (Bloco, (0, 14)), (Bloco, (1, 14)), (Bloco, (2, 14)), (Bloco, (3, 14)), (Bloco, (4, 14)), (Bloco, (6, 14)), (Caixa, (8, 14)), (Caixa, (12, 14)), (Bloco, (15, 14)), (Bloco, (16, 14)), (Bloco, (17, 14)), (Bloco, (4, 15)), (Bloco, (6, 15)), (Caixa, (8, 15)), (Bloco, (10, 15)), (Bloco, (12, 15)), (Caixa, (13, 15)), (Bloco, (15, 15)), (Bloco, (4, 16)), (Bloco, (6, 16)), (Bloco, (7, 16)), (Bloco, (8, 16)), (Bloco, (9, 16)), (Bloco, (10, 16)), (Bloco, (11, 16)), (Bloco, (12, 16)), (Bloco, (13, 16)), (Bloco, (14, 16)), (Bloco, (15, 16)), (Bloco, (4, 17)), (Bloco, (5, 17)), (Bloco, (6, 17)) ]

mapa11 :: Mapa
mapa11 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
    [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Bloco, Caixa, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  ]

lista12 :: [(Peca, Coordenadas)]
lista12 = [(Bloco, (6, 1)), (Porta, (0, 2)), (Bloco, (6, 2)), (Bloco, (2, 3)), (Caixa, (4, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4))]

mapa12 :: Mapa
mapa12 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista13 :: [(Peca, Coordenadas)]
lista13 = [(Bloco, (6, 1)), (Porta, (1, 2)), (Bloco, (6, 2)), (Bloco, (2, 3)), (Caixa, (4, 3)), (Bloco, (6, 3)), (Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)), (Bloco, (3, 4)), (Bloco, (4, 4)), (Bloco, (5, 4)), (Bloco, (6, 4))]

mapa13 :: Mapa
mapa13 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Porta, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

lista14 :: [(Peca, Coordenadas)]
lista14 = [(Bloco,(7,1)),(Porta,(1,2)),(Bloco,(2,2)),(Caixa,(6,2)),(Bloco,(7,2)),(Bloco,(2,3)),(Caixa,(5,3)),(Caixa,(6,3)),(Bloco,(7,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Bloco,(5,4)),(Bloco,(6,4)),(Bloco,(7,4))]

mapa14 :: Mapa
mapa14 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Porta, Bloco, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

jogo01 :: Jogo
jogo01 = Jogo mapa01 (Jogador (6, 0) Oeste False)

jogo02 :: Jogo
jogo02 = Jogo mapa01 (Jogador (5, 3) Oeste True)

jogo03 :: Jogo
jogo03 = Jogo mapa09 (Jogador (17, 6) Este False)

jogo04 :: Jogo
jogo04 = Jogo mapa10 (Jogador (12, 4) Este False)

jogo05 :: Jogo
jogo05 = Jogo mapa11 (Jogador (17, 11) Oeste False)

jogo06 :: Jogo
jogo06 = Jogo mapa06 (Jogador (2, 1) Oeste False)

jogo07 :: Jogo
jogo07 = Jogo mapa12 (Jogador (5, 3) Oeste True)

jogo08 :: Jogo
jogo08 = Jogo mapa13 (Jogador (5, 3) Oeste True)

jogo09 :: Jogo
jogo09 = Jogo mapa14 (Jogador (2,1) Oeste True)