module Tarefa6_2021li1g050_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g050
import Tarefa4_2021li1g050
import Tarefa6_2021li1g050
import Fixtures 


testsT6 = 
    test
      [ "Tarefa 6 - Teste Jogo " ~: Just [InterageCaixa, AndarDireita, AndarEsquerda, InterageCaixa, Trepar, Trepar, AndarEsquerda,AndarEsquerda] ~=? resolveJogo 20 m7e1
        ,"Tarefa 6 - Teste Jogo menos movimentos que int" ~: Nothing ~=? resolveJogo 5 m7e1
      ]
