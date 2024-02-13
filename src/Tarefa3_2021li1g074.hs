{- |
Module      : Tarefa3_2021li1g074
Description : Representação textual do jogo
Copyright   : Miguel Ângelo Martins Guimarães;
            : Filipe Prudêncio Pacheco dos Santos;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g074 where

import LI12122
import FuncoesAuxiliares


----------------------------------------------------------------------------------------------------------- 
-------------------------------------- Representação Textual do jogo  -------------------------------------  
----------------------------------------------------------------------------------------------------------- 


instance Show Jogo where
  show = showJogo


{- | A função faz a representação textual de um 'Jogo' , ou seja, transforma um 'Jogo' (constituido por 'Mapa' e 'Jogador' ) em uma 'String'

== Funções auxiliares:

Tem como funções auxiliares: 'colunaString', 'linhaString', 'jogadorDir', 'converteMapa'

== Exemplos de utilização:

>>> showJogo (Jogo [[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]] (Jogador (2,0) Oeste False))
"P <\nXXX"

== Propriedades:

prop> showJogo (Jogo [] (Jogador (0,0) Oeste False)) = ""
-}

showJogo :: Jogo -> String
showJogo (Jogo [] (Jogador (a,b) dir v))  = ""
showJogo (Jogo mapa (Jogador (a,b) dir v)) = colunaString (Jogo (converteMapa mapa (Jogador (a,b) dir v)) (Jogador (a,b) dir v)) 0



{- | A função transforma um 'Jogo' em uma 'String'. O funcionamento passa por aplicar a função 'linhaString' em todas as linhas do mapa construindo linha a linha a representação textual do mesmo.

== Exemplos de utilização:

>>> colunaString (Jogo [[Porta,Vazio],[Bloco,Bloco]] (Jogador (1,0) Oeste False)) 0
"P<\nXX"

== Propriedades:

prop> colunaString(Jogo [] (Jogador (0,0) Oeste False)) 0 = ""
-}

colunaString :: Jogo -> Int -> String
colunaString (Jogo [] (Jogador (a,b) dir v)) i = ""
colunaString (Jogo [x] (Jogador (a,b) dir v)) i = linhaString x (Jogador (a,b) dir v) (0,i)
colunaString (Jogo (x:xs) (Jogador (a,b) dir v)) i = linhaString x (Jogador (a,b) dir v) (0,i) ++ "\n" ++ colunaString (Jogo xs (Jogador (a,b) dir v)) (i+1)



{- | A função recebe uma determinada linha de um mapa , ou seja, uma lista de peças e recebe um 'Jogador' e um acumulador semelhante ao tipo 'Coordenadas'. Devolve transformação dessa linha do mapa em uma string.  

== Exemplos de utilização:

>>> linhaString [Porta,Caixa,Vazio,Bloco] (Jogador (2,0) Oeste False) (0,0)
"PC<X"

== Propriedades:

prop> linhaString [] (Jogador (0,0) Oeste False) (0,0) = ""
-}

linhaString :: [Peca] -> Jogador -> (Int,Int) -> String
linhaString [] _ _ = ""
linhaString (Porta:f) p (a,b) = "P" ++ linhaString f p (a+1,b)
linhaString (Caixa:f) p (a,b) = "C" ++ linhaString f p (a+1,b)
linhaString (Bloco:f) p (a,b) = "X" ++ linhaString f p (a+1,b)
linhaString (Vazio:f) (Jogador (x,y) dir v) (a,b)
    | a==x && b==y = jogadorDir dir ++ linhaString f (Jogador (x,y) dir v) (a+1,b)
    | otherwise = " " ++ linhaString f (Jogador (x,y) dir v) (a+1,b)



{- | A função recebe a 'Direcao' do jogador e devolve essa direção em 'String'.

A direção 'Oeste' corresponde á 'String' "<"

Já a direção 'Este' corresponde á string ">"

== Exemplos de utilização:

>>> jogadorDir Oeste
"<"

>>> jogadorDir Este
">"
-}

jogadorDir :: Direcao -> String
jogadorDir dir
  | dir == Oeste = "<"
  | otherwise = ">"



{- | A função faz com que, na representação textual do jogo, apareça uma caixa em cima do jogador quando este está a carregar uma 'Caixa' , ou seja, quando o booleano do jogador é True.
Para que isto aconteça é preciso alterar o 'Mapa' , convertendo o espaço Vazio em cima do jogador numa 'Caixa'

== Exemplos de utilização:

>>> converteMapa [[Vazio,Vazio],[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,1) Este True)
[[Vazio,Caixa],[Vazio,Vazio],[Bloco,Bloco]]
-}

converteMapa :: Mapa -> Jogador -> Mapa
converteMapa mapa (Jogador (x,y) dir v)
    | v = replaceMapa mapa (x,y-1,Caixa)
    | otherwise = mapa