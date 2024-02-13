module Tarefa2_2021li1g074_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g074
import Fixtures
import FuncoesAuxiliares

-- Testes da Tarefa 2
testsT2 =
  test
    [ -- Testes originais da função principal
      "Tarefa 2 - Teste Construir Mapa m1" ~: constroiMapa m1 ~=? m1r 
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort (desconstroiMapa m1r) ~=? sort m1 
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: desconstroiMapa [] ~=? [] 
    , "Tarefa 2 - Teste Identidade m1" ~: sort (desconstroiMapa (constroiMapa m1)) ~=? sort m1 
    , "Tarefa 2 - Teste Identidade m1r" ~: constroiMapa (desconstroiMapa m1r) ~=?  m1r 

    -- Funcoes auxiliares
    , "Tarefa 2 - Teste da funcao todasCoordenadas" ~: todasCoordenadas [(Porta, (7, 4)),(Bloco,(1,2))] ~=?  [(7,4),(1,2)]
    , "Tarefa 2 - Teste da funcao getYs" ~: getYs [(Bloco,(5,12)),(Bloco,(0,10))] ~=?  [12,10]
    , "Tarefa 2 - Teste da funcao getXs" ~: getXs [(Bloco,(5,12)),(Bloco,(0,10))] ~=?  [5,0] 
    , "Tarefa 2 - Teste da funcao maximumX" ~: maximumX [(Bloco,(5,5)),(Bloco,(10,10)),(Bloco,(2,2))] ~=?  10
    , "Tarefa 2 - Teste da funcao maximumY" ~: maximumY [(Bloco,(5,5)),(Bloco,(10,10)),(Bloco,(2,2))] ~=?  10
    , "Tarefa 2 - Teste da funcao contaX" ~: contaX [Bloco,Vazio,Porta,Bloco,Vazio] (0,1) ~=?  [(Bloco,(0,1)),(Porta,(2,1)),(Bloco,(3,1))]
    , "Tarefa 2 - Teste da funcao contaY" ~: contaY [[Bloco,Vazio],[Vazio,Bloco]] 0 ~=?  [(Bloco,(0,0)),(Bloco,(1,1))]    
    , "Tarefa 2 - Teste da funcao listaX" ~: listaX [(Bloco,(0,0)),(Porta,(2,1))] (0,1) ~=?  [Vazio,Vazio,Porta]    
    , "Tarefa 2 - Teste da funcao listaTudo" ~: listaTudo [(Bloco,(0,0)),(Porta,(2,1))] 0 (0,0) ~=?  [[Bloco,Vazio,Vazio],[Vazio,Vazio,Porta]]   
    
    {- Testes originais removidos 
        
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa [] 
    -- Este teste foi removido pois o mapa [] não é valido, tornando o teste invalido. 
    
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))] 
    -- Este mapa , que veio com o documento original, possui duas peças sobrepostas que o tornam invalido, pelo que iremos retirar do projeto 
         
    -}
    ]
