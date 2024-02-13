module Tarefa4_2021li1g074_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g074
import Tarefa4_2021li1g074
import Fixtures
import FuncoesAuxiliares

-- Testes da Tarefa 4
testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~:  moveJogador m1e1 AndarEsquerda ~=?  Jogo m1r (Jogador (5, 3) Oeste False) 
    , "Tarefa 4 - Teste Move m1e1 Este" ~: moveJogador m1e1 AndarDireita ~=?  Jogo m1r (Jogador (6, 0) Este False)
    , "Tarefa 4 - Teste Move m1e1 Trepar" ~: moveJogador m1e1 Trepar ~=?  m1e1
    , "Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: moveJogador m1e1 InterageCaixa  ~=? m1e1 
    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este True) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa2" ~:
      Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]

    --Testes andar esquerda
    ,"Tarefa 4 - Teste esquerda 1" ~: moveJogador (Jogo [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,0) Este False)) AndarEsquerda ~=? Jogo [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (0,0) Oeste False)
    ,"Tarefa 4 - Teste esquerda 2" ~: moveEsquerda [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,0) Este False) ~=? Jogador (0,0) Oeste False
    ,"Tarefa 4 - Teste esquerda 3" ~: moveEsquerda m2r (Jogador (6, 1) Este False) ~=? Jogador (5, 3) Oeste False
    ,"Tarefa 4 - Teste esquerda 4" ~: moveEsquerda m2r (Jogador (6, 1) Este True) ~=? Jogador (5, 3) Oeste True
    ,"Tarefa 4 - Teste esquerda 5" ~: moveEsquerda m2r (Jogador (6, 1) Oeste False) ~=? Jogador (5, 3) Oeste False
    ,"Tarefa 4 - Teste esquerda 6" ~: moveEsquerda m2r (Jogador (3, 3) Este False) ~=? Jogador (2, 3) Oeste False
    ,"Tarefa 4 - Teste esquerda 7" ~: moveEsquerda m2r (Jogador (3, 3) Este True) ~=? Jogador (2, 3) Oeste True
    ,"Tarefa 4 - Teste esquerda 8" ~: moveEsquerda m3r (Jogador (6, 1) Este False) ~=? Jogador (5, 2) Oeste False
    ,"Tarefa 4 - Teste esquerda 9" ~: moveEsquerda m3r (Jogador (6, 1) Este True) ~=? Jogador (5, 2) Oeste True


    --Testes andar direita
    ,"Tarefa 4 - Teste direita 1" ~: moveDireita m2r (Jogador (5, 2) Oeste False) ~=? Jogador (5, 2) Este False
    ,"Tarefa 4 - Teste direita 2" ~: moveDireita m2r (Jogador (1, 3) Oeste False) ~=? Jogador (2, 3) Este False
    ,"Tarefa 4 - Teste direita 3" ~: moveDireita [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (0,0) Oeste False) ~=? Jogador (1,0) Este False
    ,"Tarefa 4 - Teste direita 4" ~: moveDireita m2r (Jogador (6, 1) Oeste False) ~=? Jogador (7, 3) Este False
    ,"Tarefa 4 - Teste direita 5" ~: moveDireita m2r (Jogador (6, 1) Este True) ~=? Jogador (7, 3) Este True
    ,"Tarefa 4 - Teste direita 6" ~: moveDireita m2r (Jogador (5, 3) Oeste False) ~=? Jogador (5, 3) Este False
    ,"Tarefa 4 - Teste direita 7" ~: moveDireita m2r (Jogador (3, 3) Este False) ~=? Jogador (3, 3) Este False
    ,"Tarefa 4 - Teste direita 8" ~: moveDireita m2r (Jogador (2, 3) Oeste True) ~=? Jogador (3, 3) Este True
    ,"Tarefa 4 - Teste direita 9" ~: moveDireita m3r (Jogador (6, 1) Este False) ~=? Jogador (7, 2) Este False
    ,"Tarefa 4 - Teste direita 10" ~: moveDireita m3r (Jogador (6, 1) Este True) ~=? Jogador (7, 2) Este True

    --Testes trepar
    ,"Tarefa 4 - Teste trepar 1" ~: trepar [[Vazio,Vazio],[Vazio,Bloco],[Bloco,Bloco]] (Jogador (0,1) Este False) ~=? Jogador (1,0) Este False
    ,"Tarefa 4 - Teste trepar 2" ~: trepar [[Vazio,Vazio],[Vazio,Bloco],[Bloco,Bloco]] (Jogador (0,1) Este True) ~=? Jogador (0,1) Este True
    ,"Tarefa 4 - Teste trepar 3" ~: trepar m4r (Jogador (1, 4) Oeste False) ~=? Jogador (1, 4) Oeste False
    ,"Tarefa 4 - Teste trepar 4" ~: trepar m4r (Jogador (1, 4) Este False) ~=? Jogador (2, 3) Este False
    ,"Tarefa 4 - Teste trepar 5" ~: trepar m4r (Jogador (2, 3) Este True) ~=? Jogador (3, 2) Este True
    ,"Tarefa 4 - Teste trepar 6" ~: trepar m4r (Jogador (5, 2) Oeste False) ~=? Jogador (4, 1) Oeste False
    ,"Tarefa 4 - Teste trepar 7" ~: trepar m4r (Jogador (8, 4) Oeste False) ~=? Jogador (8, 4) Oeste False

    --Testes interage caixa
    ,"Tarefa 4 - Teste interage caixa 1" ~: interageCaixa [[Vazio,Vazio],[Vazio,Caixa],[Bloco,Bloco]] (Jogador (0,1) Este False) ~=? ([[Vazio,Vazio],[Vazio,Vazio],[Bloco,Bloco]],Jogador (0,1) Este True)
    ,"Tarefa 4 - Teste interage caixa 2" ~: interageCaixa [[Vazio,Vazio],[Vazio,Vazio],[Bloco,Bloco]] (Jogador (0,1) Este True) ~=? ([[Vazio,Vazio],[Vazio,Caixa],[Bloco,Bloco]],Jogador (0,1) Este False)
    ,"Tarefa 4 - Teste interage caixa 3" ~: interageCaixa m4r (Jogador (3,2) Este False) ~=? (m4r2,Jogador (3,2) Este True)
    ,"Tarefa 4 - Teste interage caixa 4" ~: interageCaixa m4r (Jogador (5,2) Este True) ~=? (m4r3,Jogador (5,2) Este False)

    --Testes correr movimentos
    ,"Tarefa 4 - Teste correrMovimentos 1" ~: correrMovimentos (Jogo m4r (Jogador (1,4) Este False)) [Trepar,Trepar] ~=? Jogo m4r (Jogador (3,2) Este False)
    ,"Tarefa 4 - Teste correrMovimentos 2" ~: correrMovimentos (Jogo m4r (Jogador (1,4) Este False)) [Trepar,Trepar,Trepar,AndarDireita,AndarDireita,AndarEsquerda] ~=? Jogo m4r (Jogador (6,4) Oeste False)
    ,"Tarefa 4 - Teste correrMovimentos 2" ~: correrMovimentos (Jogo m4r (Jogador (2,3) Este False)) [Trepar,InterageCaixa,AndarEsquerda,InterageCaixa] ~=? Jogo m4r4 (Jogador (2,3) Oeste False)
    ]