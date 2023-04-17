module Tarefa1_2021li1g050_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g050
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Valida Mapa 01" ~: True ~=? validaPotencialMapa lista01
    , "Valida Mapa 02" ~: True ~=? validaPotencialMapa lista02
    , "Valida Mapa 03" ~: True ~=? validaPotencialMapa lista03
    , "Valida Mapa 04" ~: True ~=? validaPotencialMapa lista04
    , "Valida Mapa 05" ~: True ~=? validaPotencialMapa lista05
    , "Valida Mapa 06" ~: True ~=? validaPotencialMapa lista06
    , "Valida Mapa 07" ~: True ~=? validaPotencialMapa lista07
    , "Valida Mapa 08" ~: True ~=? validaPotencialMapa lista08
    , "Valida Mapa 09" ~: True ~=? validaPotencialMapa lista09
    , "Valida Mapa 10" ~: True ~=? validaPotencialMapa lista10
    , "Valida Mapa 11" ~: True ~=? validaPotencialMapa lista11
    , "Valida Mapa 12" ~: True ~=? validaPotencialMapa lista12
    , "Valida Mapa 13" ~: True ~=? validaPotencialMapa lista13
    , "Valida Mapa 14" ~: True ~=? validaPotencialMapa lista14
    , "Invalido 01: não tem caminho" ~: False ~=? validaPotencialMapa invalido01
    , "Invalido 02: não tem caminho" ~: False ~=? validaPotencialMapa invalido02
    , "Invalido 03: caixa a flutuar" ~: False ~=? validaPotencialMapa invalido03
    , "Invalido 04: não tem caminho" ~: False ~=? validaPotencialMapa invalido04 -- um pouco lento
    , "Invalido 05: tem duas Portas" ~: False ~=? validaPotencialMapa invalido05
    , "Invalido 06: não tem porta" ~: False ~=? validaPotencialMapa invalido06
    , "Invalido 07: não tem espaços vazios" ~: False ~=? validaPotencialMapa invalido07
    , "Invalido 08: tem peças sobrepostas" ~: False ~=? validaPotencialMapa invalido08
    ]