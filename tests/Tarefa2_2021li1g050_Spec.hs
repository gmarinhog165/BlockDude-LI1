module Tarefa2_2021li1g050_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g050
import Fixtures

testsT2 =
  test
    [ "Construir Mapa 01" ~: mapa01 ~=? constroiMapa lista01
    , "Construir Mapa 02" ~: mapa02 ~=? constroiMapa lista02
    , "Construir Mapa 03" ~: mapa03 ~=? constroiMapa lista03
    , "Construir Mapa 04" ~: mapa04 ~=? constroiMapa lista04
    , "Construir Mapa 05" ~: mapa05 ~=? constroiMapa lista05
    , "Construir Mapa 06" ~: mapa06 ~=? constroiMapa lista06
    , "Construir Mapa 07" ~: mapa07 ~=? constroiMapa lista07
    , "Construir Mapa 08" ~: mapa08 ~=? constroiMapa lista08
    , "Construir Mapa 09" ~: mapa09 ~=? constroiMapa lista09
    , "Construir Mapa 10" ~: mapa10 ~=? constroiMapa lista10
    , "Construir Mapa 11" ~: mapa11 ~=? constroiMapa lista11
    , "Construir Mapa 12" ~: mapa12 ~=? constroiMapa lista12
    , "Construir Mapa 13" ~: mapa13 ~=? constroiMapa lista13
    , "Construir Mapa 14" ~: mapa14 ~=? constroiMapa lista14
    , "Desconstroi Mapa 01" ~: sort lista01 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa01
    , "Desconstroi Mapa 02" ~: sort lista02 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa02
    , "Desconstroi Mapa 03" ~: sort lista03 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa03
    , "Desconstroi Mapa 04" ~: sort lista04 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa04
    , "Desconstroi Mapa 05" ~: sort lista05 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa05
    , "Desconstroi Mapa 06" ~: sort lista06 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa06
    , "Desconstroi Mapa 07" ~: sort lista07 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa07
    , "Desconstroi Mapa 08" ~: sort lista08 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa08
    , "Desconstroi Mapa 09" ~: sort lista09 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa09
    , "Desconstroi Mapa 10" ~: sort lista10 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa10
    , "Desconstroi Mapa 11" ~: sort lista11 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa11
    , "Desconstroi Mapa 12" ~: sort lista12 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa12
    , "Desconstroi Mapa 13" ~: sort lista13 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa13
    , "Desconstroi Mapa 14" ~: sort lista14 ~=? (filter ((Vazio /=) . fst) . sort . desconstroiMapa) mapa14
    , "Identidade Mapa 01" ~: mapa01 ~=? (constroiMapa . desconstroiMapa) mapa01
    , "Identidade Mapa 02" ~: mapa02 ~=? (constroiMapa . desconstroiMapa) mapa02
    , "Identidade Mapa 03" ~: mapa03 ~=? (constroiMapa . desconstroiMapa) mapa03
    , "Identidade Mapa 04" ~: mapa04 ~=? (constroiMapa . desconstroiMapa) mapa04
    , "Identidade Mapa 05" ~: mapa05 ~=? (constroiMapa . desconstroiMapa) mapa05
    , "Identidade Mapa 06" ~: mapa06 ~=? (constroiMapa . desconstroiMapa) mapa06
    , "Identidade Mapa 07" ~: mapa07 ~=? (constroiMapa . desconstroiMapa) mapa07
    , "Identidade Mapa 08" ~: mapa08 ~=? (constroiMapa . desconstroiMapa) mapa08
    , "Identidade Mapa 09" ~: mapa09 ~=? (constroiMapa . desconstroiMapa) mapa09
    , "Identidade Mapa 10" ~: mapa10 ~=? (constroiMapa . desconstroiMapa) mapa10
    , "Identidade Mapa 11" ~: mapa11 ~=? (constroiMapa . desconstroiMapa) mapa11
    , "Identidade Mapa 12" ~: mapa12 ~=? (constroiMapa . desconstroiMapa) mapa12
    , "Identidade Mapa 13" ~: mapa13 ~=? (constroiMapa . desconstroiMapa) mapa13
    , "Identidade Mapa 14" ~: mapa14 ~=? (constroiMapa . desconstroiMapa) mapa14
    ]
