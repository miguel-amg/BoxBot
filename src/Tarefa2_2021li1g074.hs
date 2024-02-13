{- |
Module      : Tarefa2_2021li1g074
Description : Construção/Desconstrução do mapa
Copyright   : Miguel Ângelo Martins Guimarães;
            : Filipe Prudêncio Pacheco dos Santos;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g074 where

import LI12122
import FuncoesAuxiliares


----------------------------------------------------------------------------------------------------------- 
---------------------------------------- (1.) Construção do mapa  -----------------------------------------  
-----------------------------------------------------------------------------------------------------------

{- | A função ’constroiMapa’ transforma uma lista de pecas e as suas 'Coordenadas' em um 'Mapa'

== Funções auxiliares:
Utiliza as seguintes funções auxiliares: 'listaTudo', 'listaX', 'buscaTipo', 'todasCoordenadas', 'maximumX', 'maximumY', 'getXs' e 'getYs'
Algumas funções são importadas apartir do ficheiro FuncoesAuxiliares.hs

== Exemplos de utilização:

>>> constroiMapa [(Bloco,(1,1)),(Caixa,(2,1))]
[[Vazio,Vazio,Vazio],[Vazio,Bloco,Caixa]]
>>> constroiMapa [(Bloco,(2,2))]
[[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco]]

== Propriedades:

prop> constroiMapa [] = []
-}

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = listaTudo l 0 (0,0)



{- | A função transforma uma lista de pares (Peças e Coordenadas) e as suas 'Coordenadas' em um 'Mapa'. 
O seu funcionamento passa por aplicar a função 'listaX' para as diferentes linhas de um mapa, assim vai se contruindo o 'Mapa' linha a linha
A funcao utiliza um acumulador, que serve para contar a linha em que esta a trabalhar, e utiliza também um par de inteiros (coordenadas) igualmente como acumulador e referencia. 

== Exemplos de utilização:

>>> listaTudo [(Bloco,(2,2)] 0 (0,0)
[[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco]]


== Propriedades:

prop> listaTudo [] 0 (0,0) = []
-}

listaTudo :: [(Peca, Coordenadas)] -> Int -> (Int,Int) -> Mapa
listaTudo [] _ (_,_) = []
listaTudo l i (a,b)
    | i <= valorYmax = listaX l (a,b) : listaTudo l (i+1) (a,b+1)
    | otherwise = []
    where valorYmax = maximumY l



{- | A função transforma uma lista de par de pecas ('Peca') e as suas 'Coordenadas' em uma lista de peças, para uma determinada linha do mapa.
O funcionamento da função passa por procurar para todas as coordenadas de uma linha a peça que existe em cada uma dessas coordenadas.
Caso não haja peça numa determinada coordenada então a função insere o 'Vazio' nesse slot. 

== Exemplos de utilização:

>>> listaX [(Bloco,(2,2))] (0,2)
[Vazio,Vazio,Bloco]


== Propriedades:

prop> listaX [] (0,0) = []
-}

listaX :: [(Peca, Coordenadas)] -> (Int,Int) -> [Peca]
listaX [] _ = []
listaX l (a,b)
    | (a,b) `elem` tudo =  peca : listaX l (a+1,b)
    | otherwise = if a <= valorXmax then Vazio : listaX l (a+1,b)
                  else []
    where valorXmax = maximumX l
          tudo = todasCoordenadas l
          peca = buscaTipo l (a,b)


-----------------------------------------------------------------------------------------------------------
--------------------------------------- (2.) Desconstrução do mapa  --------------------------------------- 
-----------------------------------------------------------------------------------------------------------


{- | A função ’desconstroiMapa’ transforma um mapa em um mapa sobre a forma lista de pares , em que cada par contém uma determinada 'Peca' e as suas 'Coordenadas'
Os vazios são inseridos na lista de pares por omissão.

Possui como funções auxiliares as funções: 'contaY' e 'contaX'

== Exemplos de utilização:

>>> desconstroiMapa [[Vazio,Vazio],[Vazio,Bloco]]
[(Bloco,(1,1))]
>>> desconstroiMapa  [[Porta,Caixa],[Bloco,Bloco]]
[(Porta,(0,0)),(Caixa,(1,0)),(Bloco,(0,1)),(Bloco,(1,1))]

== Propriedades:
prop> desconstroiMapa  [] = []
-}

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = contaY mapa 0



{- | A função ’contaY’ executa a função contaX para todas as linhas de um 'Mapa' , ou seja, atráves da função 'contaX' a função contaY transforma um 'Mapa'
em um mapa sobre a forma lista de pares em que cada par contém uma determinada 'Peca' e as suas 'Coordenadas'

A função funciona recebendo um Mapa e um acumulador, ao utilizar a função deve-se colocar o acumulador igual a 0. 
Este acumulador serve para contar a linha em que se está a aplicar a função contaX.

== Exemplos de utilização:

>>> contaY [[Vazio,Vazio,Vazio],[Vazio,Bloco,Caixa]] 0
 [(Bloco,(1,1)),(Caixa,(2,1))]
>>> contaY [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco]] 0
 [(Bloco,(2,2))]

== Propriedades:

prop> contaY [] 0 = []
-}

contaY :: Mapa -> Int -> [(Peca, Coordenadas)]
contaY [] _ = []
contaY (x:xs) a = contaX x (0,a) ++ contaY xs (a+1)



{- | A função ’contaX’ transforma uma lista de peça em uma lista de pares em que cada par contém uma determinada Peça e as suas coordenadas.

A função funciona recebendo uma lista de Peças e um acumulador (que corresponde a umas coordenadas). 

O acumulador serve para referencia, isto é, á medida que a lista final é construida o acumulador vai sendo usado para associar uma determinada peça
a umas coordenadas.

== Exemplos de utilização:

>>> contaX [Caixa,Caixa] (0,1)
[(Caixa,(0,1)),(Caixa,(1,1))]

== Propriedades:

prop> contaX [] (0,0) = []
-}

contaX :: [Peca] -> (Int,Int) -> [(Peca, Coordenadas)]
contaX [] (_,_) = []
contaX (x:xs) (a,b) 
    | x /= Vazio = (x,(a,b)) : contaX xs (a+1,b)
    | otherwise = contaX xs (a+1,b)
