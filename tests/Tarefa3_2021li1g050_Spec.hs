module Tarefa3_2021li1g050_Spec where

import Test.HUnit
import Tarefa3_2021li1g050
import Fixtures

testsT3 =
  test
    [ "Show Jogo 01" ~: "      <\n      X\n      X\nP X C X\nXXXXXXX" ~=?  show jogo01
    , "Show Jogo 02" ~: "       \n      X\n     CX\nP X C<X\nXXXXXXX" ~=?  show jogo02
    , "Show Jogo 03" ~: "X                  X\nX                  X\nX                  X\nX                  X\nX                  X\nX   X       X      X\nXP  X   X C X  C > X\nXXXXXXXXXXXXXXXXXXXX" ~=? show jogo03
    , "Show Jogo 04" ~: "                    \n                    \n                    \n                    \n            >       \n    X       X       \n P  X   X C X  C    \nXXXXXXXXXXXXXXXXXXXX" ~=? show jogo04
    , "Show Jogo 04" ~: "                    \n                    \n                    \n                    \n            >       \n    X       X       \n P  X   X C X  C    \nXXXXXXXXXXXXXXXXXXXX" ~=? show jogo04
    , "Show Jogo 05" ~: "                   X     \n                  X X    \n                 X   X   \n       X        X     X  \n      X X      X       X \n   XXX   X    X         X\n  X       X  X          X\n X         XX           X\n X                      X\n X                     CX\n X                    CCX\n X               <    XXX\nXX    X          X    X  \nXP    X C        XXXXXX  \nXXXXX X C   C  XXX       \n    X X C X XC X         \n    X XXXXXXXXXX         \n    XXX                  " ~=? show jogo05
    , "Show Jogo 06" ~: "       \n  <   X\n  X   X\nP XC  X\nXXXXXXX" ~=? show jogo06
    , "Show Jogo 07" ~: "       \n      X\nP    CX\n  X C<X\nXXXXXXX" ~=? show jogo07
    , "Show Jogo 08" ~: "       \n      X\n P   CX\n  X C<X\nXXXXXXX" ~=? show jogo08
    ]
