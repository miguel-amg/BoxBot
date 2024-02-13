{- |
Module      : FuncoesAuxiliares
Description : Funções auxiliares das Tarefas
Copyright   : Miguel Ângelo Martins Guimarães;
            : Filipe Prudêncio Pacheco dos Santos;

Este modulo contém algumas das funções auxiliares utilizadas pelas funções das Tarefas 1-4

O objetivo deste modulo é simplificar o codigo e apresentar uma documentação mais limpa.
-}

module FuncoesAuxiliares where

import LI12122



{- | Função auxiliar que dado um mapa sob a forma de lista de pares devolve a 'Peca' que se encontra em determinadas coordenadas do mapa.

== Exemplo de utilização:

>>> buscaTipo [(Bloco,(1,1)),(Porta,(1,0))] (1,1)
Bloco
-}

buscaTipo :: [(Peca, Coordenadas)] -> (Int,Int) -> Peca
buscaTipo [] _ = Vazio
buscaTipo ((p, (x, y)):t) (a,b)
    | a == x && b == y = p
    | otherwise = buscaTipo t (a,b)



{- | Função auxiliar que dado um mapa sob a forma de lista de pares devolve a 'Peca' que se encontra em determinadas coordenadas do mapa sob a forma 'Just peca'

== Exemplo de utilização:

>>> buscaTipoMaybe [(Bloco,(1,1)),(Porta,(1,0))] (1,1)
Just Bloco

== Propriedades:

prop> buscaTipoMaybe [] _ = Nothing
-}

buscaTipoMaybe :: [(Peca, Coordenadas)] -> (Int,Int) -> Maybe Peca
buscaTipoMaybe [] _ = Nothing
buscaTipoMaybe ((p, (x, y)):t) (a,b)
    | a == x && b == y = Just p
    | otherwise = buscaTipoMaybe t (a,b)



{- | Função que retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento de uma lista.  

== Exemplo de utilização:

>>> delete 2 [1,2,1,2,3,1,2]
[1,1,2,3,1,2]
-}

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t)
    | x == h = t
    | otherwise = h : delete x t



{- | Função que retorna o X maximo de um mapa que se encontra sobre a forma lista de pares -}
maximumX :: [(Peca, Coordenadas)] -> Int
maximumX = maximum . getXs

{- | Função que retorna o Y maximo de um mapa que se encontra sobre a forma lista de pares -}
maximumY :: [(Peca, Coordenadas)] -> Int
maximumY = maximum . getYs

{- | Função que dado um mapa que se encontra sob a forma lista de pares devolve uma lista de inteiros contendo todas as 'Coordenadas' X dos objetos do mapa -}
getXs :: [(Peca, Coordenadas )] -> [Int]
getXs = map (fst . snd)

{- | Função que dado um mapa que se encontra sob a forma lista de pares devolve uma lista de inteiros contendo todas as 'Coordenadas' Y dos objetos do mapa -}
getYs :: [(Peca, Coordenadas )] -> [Int]
getYs = map (snd . snd)

{- | Função que retorna o maior valor da coordenada X numa lista de 'Coordenadas' -}
maximumXCoords :: [Coordenadas] -> Int
maximumXCoords = maximum . getXsCoords

{- | Função que retorna o maior valor da coordenada y numa lista de 'Coordenadas' -}
maximumYCoords :: [Coordenadas] -> Int
maximumYCoords = maximum . getYsCoords

{- | Função que dada uma lista de 'Coordenadas' devolve uma lista de inteiros de todas as 'Coordenadas' X -}
getXsCoords :: [Coordenadas] -> [Int]
getXsCoords = map fst

{- | Função que dada uma lista de 'Coordenadas' devolve uma lista de inteiros de todas as 'Coordenadas' Y -}
getYsCoords :: [Coordenadas] -> [Int]
getYsCoords = map snd


-----------------------------------------------------------------------------------------------------------
-------------------------------------- Funções Auxiliares Tarefa 2  --------------------------------------- 
-----------------------------------------------------------------------------------------------------------

{- | Função que dado um mapa que se encontra sob a forma lista de pares devolve uma lista de 'Coordenadas' de todos os objetos do mapa.

== Exemplo de utilização:

>>> todasCoordenadas [(Caixa,(2,2)),(Bloco,(3,3))]
[(2,2),(3,3)]
-}

todasCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
todasCoordenadas [] = []
todasCoordenadas ((p,coordenadas):xs) = coordenadas : todasCoordenadas xs



-----------------------------------------------------------------------------------------------------------
-------------------------------------- Funções Auxiliares Tarefa 4  --------------------------------------- 
-----------------------------------------------------------------------------------------------------------

{- | Função que dado um 'Mapa' troca uma 'Peca' numas dadas 'Coordenadas' por uma outra 'Peca'
Utiliza a função auxiliar 'replace'

== Exemplo de utilização:

>>> replaceMapa [[Bloco,Porta],[Bloco,Caixa]] (1,1,Vazio)
[[Bloco,Porta],[Bloco,Vazio]]

== Propriedades:

prop> replaceMapa [] = []
-}

replaceMapa :: Mapa -> (Int, Int, Peca) -> Mapa
replaceMapa [] _ = []
replaceMapa (h:t) (x,y,peca)
    | y == 0    = replace h (x,peca) : t
    | y < 0     = h : t
    | otherwise = h : replaceMapa t (x,y-1,peca)



{- | Função auxiliar que dada uma lista de pecas, troca uma 'Peca' num dado valor X por uma outra 'Peca'.

== Exemplo de utilização:

>>> replace [Bloco,Porta] (1,Vazio)
[Bloco,Vazio]

== Propriedades:

prop> replace [] = []
-}

replace :: [Peca] -> (Int,Peca) -> [Peca]
replace [] _ = []
replace (h:t) (x,peca)
    | x == 0    = peca : t
    | x < 0     = h : t
    | otherwise = h : replace t (x-1,peca)



{- | Função que dado um 'Mapa' e umas 'Coordenadas' (que serve como referencia) encontra o primeiro 'Bloco' ou 'Caixa' numa linha vertical e retorna as 'Coordenadas' do espaço acima.

== Exemplo de utilização:

>>> buscaChao [[Vazio,Vazio],[Vazio,Vazio],[Bloco,Caixa]] (0,0)
(0,1)
-}

buscaChao :: Mapa -> Coordenadas -> Coordenadas
buscaChao m (x,y)
    | blocoOuCaixa = (x,y-1)
    | otherwise = buscaChao m (x,y+1)
    where blocoOuCaixa = buscaTipoMapa m (x,y) == Just Bloco || buscaTipoMapa m (x,y) == Just Caixa



{- | Função auxiliar que dado um 'Mapa' retorna o tipo de uma 'Peca' para umas dadas 'Coordenadas' sob a forma de 'Just Peca' sendo semelhante à função 'buscaTipoMaybe'

Utiliza como funções auxiliares: 'buscaLinha' e 'buscaX'

== Exemplo de utilização:

>>> buscaTipoMapa [[Bloco,Porta],[Bloco,Caixa]] (1,1)
Just Caixa
-}

buscaTipoMapa :: Mapa -> (Int, Int) -> Maybe Peca
buscaTipoMapa m (x,y) = buscaX (buscaLinha m y) x



{- | Função auxiliar que retorna uma certa linha (uma lista de pecas) do mapa, para um dado valor Y. Exemplo : Se Y = 0 então retorna a primeira linha.

== Exemplo de utilização:

>>> buscaLinha [[Bloco,Porta],[Bloco,Caixa]] 1
[Bloco,Caixa]
-}

buscaLinha :: Mapa -> Int -> [Peca]
buscaLinha [] _ = []
buscaLinha (h:t) y
    | y == 0 = h
    | otherwise = buscaLinha t (y-1)



{- | Função auxiliar que retorna uma determinada 'Peca' dado um valor x numa linha do mapa. Exemplo: Se x = 0 então retorna a primeira 'Peca'

== Exemplo de utilização:

>>> buscaX [Bloco,Porta] 1
Just Porta
-}

buscaX :: [Peca] -> Int -> Maybe Peca
buscaX [] _ = Nothing
buscaX (h:t) x
    | x == 0 = Just h
    | otherwise = buscaX t (x-1)


-----------------------------------------------------------------------------------------------------------
-------------------------------------- Funções Auxiliares Fase 2  ----------------------------------------- 
-----------------------------------------------------------------------------------------------------------


{- | Função que indica se o jogo terminou.
A função retorna True caso o jogador se encontre nas mesmas coordenadas da porta caso contrário retorna False.
Utiliza as funções auxiliares 'gameOverAux' e 'buscaPorta'
-}
gameOver :: Jogo -> Bool
gameOver (Jogo mapa (Jogador (a,b) dir val)) = gameOverAux (buscaPorta  mapa) (Jogo mapa (Jogador (a,b) dir val))

-- | Função auxiliar que verifica se as coordenadas do Jogador são iguais a um outro par de coordenadas.
gameOverAux :: Coordenadas -> Jogo -> Bool
gameOverAux (x,y) (Jogo mapa (Jogador (a,b) dir val)) = (x,y) == (a,b)

-- | Função que encontra as Coordenadas da Porta, dado um mapa
buscaPorta :: Mapa -> (Int, Int)
buscaPorta mapa = (buscaPortaX a,b)
    where (a,b) = buscaPortaY mapa 0

-- | Função auxiliar que encontra o valor do eixo dos Y em que a porta se encontra e devolve um par do Int e da Lista de Pecas correspondentes
buscaPortaY :: Mapa -> Int -> ([Peca], Int)
buscaPortaY [] _ = error "Mapa inválido, não existe porta"
buscaPortaY (h:t) n
    | Porta `elem` h = (h,n)
    | otherwise = buscaPortaY t (n+1)

-- | Função auxiliar que dada uma lista de Pecas encontra o valor do eixo dos X em que a porta se encontra
buscaPortaX :: [Peca] -> Int
buscaPortaX [] = error "Mapa inválido, não existe porta"
buscaPortaX (h:t)
    | h == Porta = 0
    | otherwise = 1 + buscaPortaX t

-- | Função auxiliar que devolve as coordenadas das Caixas de uma lista de Pecas e Coordenadas
listaCaixas :: [(Peca, Coordenadas)] -> [Coordenadas]
listaCaixas [] = []
listaCaixas ((p,(x,y)):t)
    | p == Caixa = (x,y) : listaCaixas t
    | otherwise = listaCaixas t


{- Não utilizado
Funções semelhantes às definidas em cima, desta vez para trabalhar com valores do tipo Maybe.

maximumYmaybe :: [Maybe Coordenadas] -> Int
maximumYmaybe l = maximum (getXsMaybe l)
maximumXmaybe :: [Maybe Coordenadas] -> Int
maximumXmaybe l = maximum (getXsMaybe l)

getXsMaybe ::  [Maybe Coordenadas] -> [Int]
getXsMaybe [] = []
getXsMaybe (Nothing : t) = getXsMaybe t
getXsMaybe ((Just (x,y)) : t) = x : getXsMaybe t

getYsMaybe ::  [Maybe Coordenadas] -> [Int]
getYsMaybe [] = []
getYsMaybe (Nothing : t) = getYsMaybe t
getYsMaybe ((Just (x,y)) : t) = y : getYsMaybe t
-}
