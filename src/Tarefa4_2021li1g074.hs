{- |
Module      : Tarefa4_2021li1g074
Description : Movimentação do personagem
Copyright   : Miguel Ângelo Martins Guimarães;
            : Filipe Prudêncio Pacheco dos Santos;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g074 where

import LI12122
import FuncoesAuxiliares
import Tarefa3_2021li1g074
import Tarefa2_2021li1g074 (constroiMapa)


----------------------------------------------------------------------------------------------------------- 
--------------------------------------- Movimentação da personagem  ---------------------------------------  
----------------------------------------------------------------------------------------------------------- 

{- | A função aplica o efeito de um comando (i.e. Movimento) sobre o 'Jogador' , e a sua generalização.

== Funções auxiliares:

Utiliza como funções auxiliares: 'moveEsquerda', 'moveDireita', 'trepar', 'interageCaixa'

== Exemplo de utilização:

>>> moveJogador (Jogo [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,0) Este False)) AndarEsquerda
Jogo [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (0,0) Oeste False)
-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador (a,b) dir val)) movimento =
    case movimento of AndarEsquerda -> Jogo mapa (moveEsquerda mapa (Jogador (a,b) dir val))
                      AndarDireita  -> Jogo mapa (moveDireita  mapa (Jogador (a,b) dir val))
                      Trepar        -> Jogo mapa (trepar       mapa (Jogador (a,b) dir val))
                      InterageCaixa -> Jogo mapa2 jogadorInterage
                      where mapa2           = fst (interageCaixa mapa (Jogador (a,b) dir val))
                            jogadorInterage = snd (interageCaixa mapa (Jogador (a,b) dir val))



{- | A função aplica consecutivamente os comandos dados por uma lista de movimentos.

== Exemplo de utilização:

>>> correrMovimentos (Jogo [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,0) Este False)) [AndarEsquerda,AndarDireita]
Jogo [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,0) Este False)
-}

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos = foldl moveJogador



----------------------------------------------------------------------------------------------------------- 
---------------------------------------- Movimento para a Esquerda  ---------------------------------------  
----------------------------------------------------------------------------------------------------------- 

{- | Função auxiliar que aplica o movimento AndarEsquerda sobre o jogador, alterando as suas coordenadas e a sua direção (quando o movimento é válido).
Utiliza as funções 'buscaTipoMapa' e 'buscaChao' 

== Exemplo de utilização:

>>> moveEsquerda [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,0) Este False)
Jogador (0,0) Oeste False
-}

moveEsquerda :: Mapa -> Jogador -> Jogador

moveEsquerda m (Jogador (x, y) dir False)
    | caminhoVazio  = Jogador (x-1, y) Oeste False
    | caminhoBuraco = Jogador (buscaChao m (x-1,y+1)) Oeste False
    | otherwise     = Jogador (x, y) Oeste False
    where caminhoVazio  = (buscaTipoMapa m (x-1,y) == Just Vazio || buscaTipoMapa m (x-1, y) == Just Porta) && buscaTipoMapa m (x-1,y+1) /= Just Vazio && buscaTipoMapa m (x-1,y+1) /= Just Porta
          caminhoBuraco = buscaTipoMapa m (x-1,y) == Just Vazio && (buscaTipoMapa m (x-1,y+1) == Just Vazio || buscaTipoMapa m (x-1,y+1) == Just Porta)

moveEsquerda m (Jogador (x, y) dir True)
    | caminhoVazio  = Jogador (x-1, y) Oeste True
    | caminhoBuraco = Jogador (buscaChao m (x-1,y+1)) Oeste True
    | otherwise     = Jogador (x, y) Oeste True
    where caminhoVazio  = (buscaTipoMapa m (x-1,y) == Just Vazio || buscaTipoMapa m (x-1, y) == Just Porta) && buscaTipoMapa m (x-1,y+1) /= Just Vazio && buscaTipoMapa m (x-1,y+1) /= Just Porta && buscaTipoMapa m (x-1, y-1) == Just Vazio
          caminhoBuraco = buscaTipoMapa m (x-1,y) == Just Vazio && (buscaTipoMapa m (x-1,y+1) == Just Vazio || buscaTipoMapa m (x-1,y+1) == Just Porta) && buscaTipoMapa m (x-1, y-1) == Just Vazio



----------------------------------------------------------------------------------------------------------- 
---------------------------------------- Movimento para a Direita  ----------------------------------------  
----------------------------------------------------------------------------------------------------------- 

{- | Função auxiliar que aplica o movimento AndarDireita sobre o jogador, alterando as suas coordenadas e a sua direção (quando o movimento é válido).
Utiliza as funções 'buscaTipoMapa' e 'buscaChao' 

== Exemplo de utilização:

>>> moveDireita [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (0,0) Oeste False)
Jogador (1,0) Este False
-}

moveDireita :: Mapa -> Jogador -> Jogador

moveDireita m (Jogador (x, y) dir False)
    | caminhoVazio  = Jogador (x+1, y) Este False
    | caminhoBuraco = Jogador (buscaChao m (x+1,y+1)) Este False
    | otherwise     = Jogador (x, y) Este False
    where caminhoVazio  = (buscaTipoMapa m (x+1,y) == Just Vazio || buscaTipoMapa m (x+1, y) == Just Porta) && buscaTipoMapa m (x+1,y+1) /= Just Vazio && buscaTipoMapa m (x+1,y+1) /= Just Porta
          caminhoBuraco = buscaTipoMapa m (x+1,y) == Just Vazio && (buscaTipoMapa m (x+1,y+1) == Just Vazio || buscaTipoMapa m (x+1,y+1) == Just Porta)

moveDireita m (Jogador (x, y) dir True)
    | caminhoVazio  = Jogador (x+1, y) Este True
    | caminhoBuraco = Jogador (buscaChao m (x+1,y+1)) Este True
    | otherwise     = Jogador (x, y) Este True
    where caminhoVazio  = (buscaTipoMapa m (x+1,y) == Just Vazio || buscaTipoMapa m (x+1, y) == Just Porta) && buscaTipoMapa m (x+1,y+1) /= Just Vazio && buscaTipoMapa m (x+1,y+1) /= Just Porta && buscaTipoMapa m (x+1, y-1) == Just Vazio
          caminhoBuraco = buscaTipoMapa m (x+1,y) == Just Vazio && (buscaTipoMapa m (x+1,y+1) == Just Vazio || buscaTipoMapa m (x+1,y+1) == Just Porta) && buscaTipoMapa m (x+1, y-1) == Just Vazio

----------------------------------------------------------------------------------------------------------- 
-------------------------------------------- Movimento "Trepar" -------------------------------------------
----------------------------------------------------------------------------------------------------------- 

{- | Função auxiliar que aplica o movimento Trepar sobre o jogador, alterando as suas coordenadas (quando o movimento é válido).
Utiliza a função 'buscaTipoMapa'

== Exemplo de utilização:

>>> trepar [[Vazio,Vazio],[Vazio,Bloco],[Bloco,Bloco]] (Jogador (0,1) Este False)
Jogador (1,0) Este False

>>> trepar [[Vazio,Vazio],[Vazio,Bloco],[Bloco,Bloco]] (Jogador (0,1) Este True)
Jogador (0,1) Este True  -- Por estar a carregar uma caixa e não haver espaço para esta, o movimento já não é válido.
-}

trepar :: Mapa -> Jogador -> Jogador

trepar m (Jogador (x, y) Oeste False)
    | caminhoLivreEsquerda = Jogador (x-1, y-1) Oeste False
    | otherwise            = Jogador (x, y) Oeste False
    where caminhoLivreEsquerda = (buscaTipoMapa m (x-1,y-1) == Just Vazio || buscaTipoMapa m (x-1, y-1) == Just Porta) && (buscaTipoMapa m (x-1,y) == Just Caixa || buscaTipoMapa m (x-1,y) == Just Bloco)

trepar m (Jogador (x, y) Este False)
    | caminhoLivreDireita = Jogador (x+1, y-1) Este False
    | otherwise           = Jogador (x, y) Este False
    where caminhoLivreDireita = (buscaTipoMapa m (x+1,y-1) == Just Vazio || buscaTipoMapa m (x+1, y-1) == Just Porta) && (buscaTipoMapa m (x+1,y) == Just Caixa || buscaTipoMapa m (x+1,y) == Just Bloco)

trepar m (Jogador (x, y) Oeste True)
    | caminhoLivreEsquerda = Jogador (x-1, y-1) Oeste True
    | otherwise            = Jogador (x, y) Oeste True
    where caminhoLivreEsquerda = (buscaTipoMapa m (x-1,y-1) == Just Vazio || buscaTipoMapa m (x-1, y-1) == Just Porta) && buscaTipoMapa m (x-1, y-2) == Just Vazio && (buscaTipoMapa m (x-1,y) == Just Caixa || buscaTipoMapa m (x-1,y) == Just Bloco)

trepar m (Jogador (x, y) Este True)
    | caminhoLivreDireita = Jogador (x+1, y-1) Este True
    | otherwise           = Jogador (x, y) Este True
    where caminhoLivreDireita = (buscaTipoMapa m (x+1,y-1) == Just Vazio || buscaTipoMapa m (x+1, y-1) == Just Porta) && buscaTipoMapa m (x+1, y-2) == Just Vazio && (buscaTipoMapa m (x+1,y) == Just Caixa || buscaTipoMapa m (x+1,y) == Just Bloco)



----------------------------------------------------------------------------------------------------------- 
------------------------------------------ Interagir com a caixa ------------------------------------------
----------------------------------------------------------------------------------------------------------- 

{- | Função auxiliar que aplica o movimento InterageCaixa sobre o jogador e sobre o mapa, devolvendo um par composto por um mapa e pelo jogador.
A caixa com que o jogador interage desaparece do mapa e, simultaneamente, o booleano deste passa a True.
Inversamente, da próxima vez que o jogador interage com a caixa (i.e. decide pousá-la), o booleano do Jogador passa a False e a caixa aparece no mapa.

Utiliza as funções 'buscaTipoMapa', 'replaceMapa' e 'buscaChao' 

== Exemplo de utilização:

>>> interageCaixa [[Vazio,Vazio],[Vazio,Caixa],[Bloco,Bloco]] (Jogador (0,1) Este False)
([[Vazio,Vazio],[Vazio,Vazio],[Bloco,Bloco]],Jogador (0,1) Este True)
-}

interageCaixa :: Mapa -> Jogador -> (Mapa, Jogador)

interageCaixa m (Jogador (x, y) Oeste False)
    | caixaEsquerda = (m1, Jogador (x, y) Oeste True)
    | otherwise     = (m , Jogador (x, y) Oeste False)
    where caixaEsquerda = buscaTipoMapa m (x-1,y) == Just Caixa && buscaTipoMapa m (x,y-1) == Just Vazio && buscaTipoMapa m (x-1,y-1) == Just Vazio
          m1            = replaceMapa m (x-1, y, Vazio)

interageCaixa m (Jogador (x, y) Oeste True)
    | vazioEsquerda       = (m2, Jogador (x, y) Oeste False)
    | vazioBuracoEsquerda = (m5, Jogador (x, y) Oeste False)
    | algoEsquerda        = (m7, Jogador (x, y) Oeste False)
    | otherwise           = (m , Jogador (x, y) Oeste True)
    where vazioEsquerda       = buscaTipoMapa m (x-1,y) == Just Vazio && buscaTipoMapa m (x-1,y+1) /= Just Vazio
          vazioBuracoEsquerda = buscaTipoMapa m (x-1,y) == Just Vazio && buscaTipoMapa m (x-1,y+1) == Just Vazio
          algoEsquerda        = (buscaTipoMapa m (x-1,y) == Just Caixa || buscaTipoMapa m (x-1,y) == Just Bloco) && buscaTipoMapa m (x-1,y-1) == Just Vazio 
          m2                  = replaceMapa m (x-1, y, Caixa)
          m5                  = replaceMapa m (a, b, Caixa)
          m7                  = replaceMapa m (x-1, y-1, Caixa)
          (a,b)               = buscaChao m (x-1,y+1)

interageCaixa m (Jogador (x, y) Este False)
    | caixaDireita = (m3, Jogador (x, y) Este True)
    | otherwise    = (m, Jogador (x, y) Este False)
    where caixaDireita = buscaTipoMapa m (x+1,y) == Just Caixa && buscaTipoMapa m (x,y-1) == Just Vazio && buscaTipoMapa m (x+1,y-1) == Just Vazio
          m3           = replaceMapa m (x+1, y, Vazio)

interageCaixa m (Jogador (x, y) Este True)
    | vazioDireita       = (m4, Jogador (x, y) Este False)
    | vazioBuracoDireita = (m6, Jogador (x, y) Este False)
    | algoDireita        = (m8, Jogador (x, y) Este False)
    | otherwise          = (m, Jogador (x, y) Este True)
    where vazioDireita       = buscaTipoMapa m (x+1,y) == Just Vazio && buscaTipoMapa m (x+1,y+1) /= Just Vazio
          vazioBuracoDireita = buscaTipoMapa m (x+1,y) == Just Vazio && buscaTipoMapa m (x+1,y+1) == Just Vazio
          algoDireita        = (buscaTipoMapa m (x+1,y) == Just Caixa || buscaTipoMapa m (x+1,y) == Just Bloco) && buscaTipoMapa m (x+1,y-1) == Just Vazio 
          m4                 = replaceMapa m (x+1, y, Caixa)
          m6                 = replaceMapa m (c, d, Caixa)
          m8                 = replaceMapa m (x+1, y-1, Caixa)
          (c,d)              = buscaChao m (x+1,y+1)