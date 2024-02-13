module Tarefa1_2021li1g074_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g074
import Tarefa2_2021li1g074 (desconstroiMapa)
import Fixtures
import FuncoesAuxiliares

-- Testes da Tarefa 1
testsT1 =
  test
    [ -- Testes originais da função principal
      "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False

    -- Testes da função principal validaPotencialMapa
    , "Tarefa 1 - Validar mapa sem portas" ~: validaPotencialMapa mapaSP ~=?  False
    , "Tarefa 1 - Validar mapa com peças sobrepostas" ~: validaPotencialMapa [(Bloco, (2,2)), (Porta, (2,2))] ~=?  False
    , "Tarefa 1 - Validar mapa com uma caixa flutuante" ~: validaPotencialMapa mapaCF ~=?  False
    , "Tarefa 1 - Validar mapa com varias caixas empilhadas" ~: validaPotencialMapa mapaCF2 ~=?  True
    , "Tarefa 1 - Validar mapa com uma caixa empilhada e uma a flutuar" ~: validaPotencialMapa [(Bloco,(0,4)),(Caixa ,(0,2)),(Caixa,(0,1))] ~=?  False
    , "Tarefa 1 - Validar mapa com chao continuo n1" ~: existeChao mapa1Faq  ~=? True
    , "Tarefa 1 - Validar mapa com chao continuo n2" ~: existeChao m1 ~=? True
    , "Tarefa 1 - Validar mapa com chao descontinuo" ~: existeChao mapa1FaqDescontinuo ~=? False
       
    -- Testes das funções auxiliares
    , "Tarefa 1 - Teste da funcao existirVazio (mapa vazio)" ~: existirVazio [] ~=? False
    , "Tarefa 1 - Teste da funcao existirVazio (mapa com vazios)" ~: existirVazio [(Bloco, (0,0)), (Vazio, (1,0))] ~=? True
    , "Tarefa 1 - Teste da funcao existirVazio (mapa sem vazios)" ~: existirVazio [(Bloco, (0,0)), (Bloco, (1,0))] ~=? False
    , "Tarefa 1 - Teste da funcao existirVazio (mapa Vazios por omissao)" ~: existirVazio [(Bloco, (3,3))] ~=? True

    , "Tarefa 1 - Teste da funcao existeChao mapatecto" ~: existeChao (desconstroiMapa mapatecto) ~=? False
    , "Tarefa 1 - Teste da funcao blocosConnectados" ~: blocosConectados (listaBlocosMaybe (desconstroiMapa mapatecto)) (Just (2,0)) ~=? [Just (1,0),Just (2,0),Just (3,0)]
    , "Tarefa 1 - Teste da funcao blocosConnectados" ~: blocosConectados [Just (2,0),Just (4,0)] (Just (2,0)) ~=? [Just (2,0)]

    , "Tarefa 1 - Teste da funcao existeChao 1" ~: existeChao (desconstroiMapa mapaContinuo) ~=? True
    , "Tarefa 1 - Teste da funcao existeChao 2" ~: existeChao (desconstroiMapa mapaBlocosSoltos) ~=? False
    , "Tarefa 1 - Teste da funcao existeChao 3" ~: existeChao (desconstroiMapa mapaZigZag) ~=? True
    , "Tarefa 1 - Teste da funcao existeChao 4" ~: existeChao (desconstroiMapa mapaZigZag2) ~=? False
    ]
