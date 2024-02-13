module Tarefa3_2021li1g074_Spec where

import Test.HUnit
import Tarefa3_2021li1g074
import Fixtures
import LI12122
import FuncoesAuxiliares

-- Testes da Tarefa 3
testsT3 =
  test
    [ -- Testes originais
      "Tarefa 3 - Teste Imprime Jogo m1e1" ~: show m1e1 ~=? "      <\n      X\n      X\nP   C X\nXXXXXXX" 
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: show m1e2 ~=? "       \n      X\n      X\nP < C X\nXXXXXXX" 

    -- Testes pessoais
    , "Tarefa 3 - Teste Imprime Jogo mapaEscadaShow" ~: show mapaEscadaShow ~=? "       \n   X   \n  XXX  \nPXXXXX<\nXXXXXXX"
    , "Tarefa 3 - Teste Alt nº1" ~: show (Jogo [[Vazio],[Caixa],[Caixa],[Bloco]] (Jogador (0, 0) Oeste False)) ~=? "<\nC\nC\nX"
    , "Tarefa 3 - Teste Alt nº2" ~: show (Jogo [[Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco]] (Jogador (0, 0) Oeste False)) ~=? "<   \nXXXX"

    -- Testes das funções auxiliares
    , "Tarefa 3 - Teste da função auxiliar converteMapa" ~: converteMapa [[Vazio,Vazio],[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,1) Este True) ~=? [[Vazio,Caixa],[Vazio,Vazio],[Bloco,Bloco]]
    , "Tarefa 3 - Teste da função auxiliar jogadorDir" ~: jogadorDir Oeste ~=? "<"
    , "Tarefa 3 - Teste nº2 da função auxiliar jogadorDir" ~: jogadorDir Este ~=? ">"
    , "Tarefa 3 - Teste da função auxiliar linhaString" ~: linhaString [Porta,Caixa,Vazio,Bloco] (Jogador (2,0) Oeste False) (0,0) ~=? "PC<X"
    , "Tarefa 3 - Teste da função auxiliar colunaString" ~: colunaString (Jogo [[Porta,Vazio],[Bloco,Bloco]] (Jogador (1,0) Oeste False)) 0~=? "P<\nXX"
    ]