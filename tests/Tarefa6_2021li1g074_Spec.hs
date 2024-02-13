module Tarefa6_2021li1g074_Spec where

import Test.HUnit
import LI12122
import Fixtures

import FuncoesAuxiliares
import Tarefa6_2021li1g074

-- Testes da Tarefa 6
testsT6 =
  test
    [
      "Tarefa 6 - Teste xxxxxxx1" ~: resolveJogo 4 jogoD1    ~=? Just [AndarDireita,AndarDireita,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste xxxxxxx2" ~: resolveJogo 9 jogoD2    ~=? Just [AndarDireita,AndarDireita,Trepar,AndarDireita,AndarDireita,Trepar,AndarDireita,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste xxxxxxx3" ~: resolveJogo 13 jogoE1   ~=? Just [AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste xxxxxxx4" ~: resolveJogo 100 jogoE2  ~=? Nothing
    , "Tarefa 6 - Teste xxxxxxx5" ~: resolveJogo 14 jogoC1   ~=? Just [AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste xxxxxxx6" ~: resolveJogo 29 jogoC2   ~=? Just [AndarEsquerda,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,InterageCaixa,AndarEsquerda,Trepar,AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste xxxxxxx7" ~: resolveJogo 24 jogobv2c ~=? Just [AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste xxxxxxx8" ~: resolveJogo 26 jogobbb  ~=? Just [AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,Trepar,AndarDireita]
    , "Tarefa 6 - Teste xxxxxxx9" ~: resolveJogo 95 jogobbb2 ~=? Just [AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,Trepar,AndarDireita]
    , "Tarefa 6 - Teste xxxxxxx10" ~: resolveJogo 24 jogo5   ~=? Just [AndarEsquerda,Trepar,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,Trepar,AndarDireita]
    ]