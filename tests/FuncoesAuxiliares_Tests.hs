module FuncoesAuxiliares_Tests where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g074
import Fixtures
import FuncoesAuxiliares

-- Testes das Funções auxiliares
testsAux =
  test
    [ -- Funções auxiliares da Tarefa 1
      
      "Funçoes Aux - Função buscaTipo" ~: buscaTipo [(Bloco,(1,1)),(Caixa,(1,0))] (1,0) ~=? Caixa
    , "Funçoes Aux - Função buscaTipoMaybe" ~: buscaTipoMaybe [] (0,0) ~=? Nothing
    , "Funçoes Aux - Função buscaTipoMaybe" ~: buscaTipoMaybe [(Bloco,(1,1)),(Caixa,(1,0))] (1,0) ~=? Just Caixa
    , "Funçoes Aux - Função delete" ~: delete Caixa [Caixa,Bloco,Porta,Caixa] ~=? [Bloco,Porta,Caixa]
    , "Funçoes Aux - Função delete" ~: delete Caixa [] ~=? []
    , "Funçoes Aux - Função delete" ~: delete Caixa [Bloco,Porta,Caixa] ~=? [Bloco,Porta]

    -- Funções que trabalham com coordenadas
    , "Funçoes Aux - maximumX" ~: maximumX [(Caixa,(2,2))] ~=? 2
    , "Funçoes Aux - maximumY" ~: maximumY [(Caixa,(2,2))] ~=? 2
    , "Funçoes Aux - getXs" ~: getXs [(Caixa,(2,2)),(Porta,(3,2))] ~=? [2,3]
    , "Funçoes Aux - getYs" ~: getYs [(Caixa,(2,2)),(Porta,(3,3))] ~=? [2,3]
    , "Funçoes Aux - getYs" ~: getYs [(Caixa,(2,2)),(Porta,(3,3))] ~=? [2,3]
    , "Funçoes Aux - maximumXCoords" ~: maximumXCoords [(2,2),(7,7),(2,2),(3,3)] ~=? 7
    , "Funçoes Aux - maximumYCoords" ~: maximumYCoords [(2,2),(7,7),(2,2),(3,3)] ~=? 7
    , "Funçoes Aux - getXsCoords" ~: getXsCoords [(2,2),(7,7),(2,2),(3,3)] ~=? [2,7,2,3]
    , "Funçoes Aux - getYsCoords" ~: getYsCoords [(2,2),(7,7),(2,2),(3,3)] ~=? [2,7,2,3]

    -- Funções auxiliares Tarefa 2
    , "Funçoes Aux - todasCoordenadas" ~: todasCoordenadas [(Caixa,(2,2)),(Porta,(2,1))] ~=? [(2,2),(2,1)]
    
    -- Funções auxiliares Tarefa 4
    , "Funçoes Aux - replaceMapa" ~: replaceMapa [[Caixa,Caixa],[Bloco,Caixa]] (0,1,Caixa) ~=? [[Caixa,Caixa],[Caixa,Caixa]] 
    , "Funçoes Aux - replace" ~: replace [Bloco,Caixa] (0,Caixa) ~=? [Caixa,Caixa]
    , "Funçoes Aux - buscaChao" ~: buscaChao [[Vazio,Caixa],[Bloco,Bloco]] (0,0) ~=? (0,0)
    , "Funçoes Aux - buscaTipoMapa" ~: buscaTipoMapa [[Bloco,Porta],[Bloco,Caixa]] (1,1) ~=? Just Caixa
    , "Funçoes Aux - buscaLinha" ~:  buscaLinha [[Bloco,Porta],[Bloco,Caixa]] 1 ~=? [Bloco,Caixa]
    , "Funçoes Aux - buscaX" ~:  buscaX [Bloco,Porta] 1 ~=? Just Porta
    ]
