{- |
Module      : Tarefa1_2021li1g074
Description : Validação de um potencial mapa
Copyright   : Miguel Ângelo Martins Guimarães;
            : Filipe Prudêncio Pacheco dos Santos;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g074 where

import LI12122
import FuncoesAuxiliares

{- | Função que utiliza as Funções 'sobreposicao', 'caixasFlutuantes', 'soUmaPorta', 'existirVazio' e 'existeChao' para validar o mapa, sendo o mapa considerado válido quando estas retornão o booleano True. -}
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = sobreposicao pecas && caixasFlutuantes pecas && soUmaPorta pecas && existirVazio pecas && existeChao pecas



-----------------------------------------------------------------------------------------------------------
---------------------------------- (1.) Verificação de peças sobrepostas  --------------------------------- 
-----------------------------------------------------------------------------------------------------------

{- | A função ’sobreposicao’ serve para verificar se um determinado mapa possui peças sobrepostas, isto é, duas ou mais peças com a mesma coordenada.
Caso haja alguma peça com as mesmas 'Coordenadas' que uma outra então a função retorna o valor 'False' caso não se verifique nenhuma peça sobreposta a função retorna 'True'

Possui como função auxiliar a função 'todasCoordenadas'

== Exemplos de utilização:

>>> sobreposicao [(Bloco,(1,1)),(Caixa,(1,1))]
False
>>> sobreposicao [(Bloco,(1,1)),(Caixa,(2,1))]
True

== Propriedades:

prop> sobreposicao [] = True
-}

sobreposicao :: [(Peca, Coordenadas)] -> Bool
sobreposicao [] = True
sobreposicao ((p,coords):xs)
    | coords `elem` todasCoordenadas xs = False
    | otherwise = sobreposicao xs



-----------------------------------------------------------------------------------------------------------
-------------------------------------- (2.) Verificação das portas  ---------------------------------------
-----------------------------------------------------------------------------------------------------------

{- | Função que verifica se só existe uma porta num dado mapa sob a forma lista de pares (Coordenadas e Peças).

Utiliza como função auxiliar: 'soUmaPortaAux'

== Exemplo de utilização:

>>> soUmaPorta [(Bloco,(1,1)),(Porta,(1,0))]
True

>>> soUmaPorta [(Porta,(1,1)),(Porta,(1,0))]
False

== Propriedades:

prop> soUmaPorta [] = False
-}

soUmaPorta :: [(Peca, Coordenadas)] -> Bool
soUmaPorta [] = False
soUmaPorta ((a, xs):t) = soUmaPortaAux ((a, xs):t) == 1



{- | Função auxiliar que conta o numero de portas numa lista de pecas.

== Exemplo de utilização:

>>> soUmaPortaAux [(Bloco,(1,1)),(Porta,(1,0))]
1

>>> soUmaPortaAux [(Porta,(1,1)),(Porta,(1,0))]
2

== Propriedades:

prop> soUmaPorta [] = 0
-}

soUmaPortaAux :: [(Peca, Coordenadas)] -> Int
soUmaPortaAux [] = 0
soUmaPortaAux ((a, xs):t)
    | a == Porta = 1 + soUmaPortaAux t
    | otherwise = soUmaPortaAux t



-----------------------------------------------------------------------------------------------------------
---------------------------------- (3.) Verificação de caixas flutuantes ---------------------------------- 
-----------------------------------------------------------------------------------------------------------

{- | A função ’caixasFlutuantes’ serve para verificar se em um determinado mapa existem caixas que se encontrem a flutuar, isto é, que não se encontrem em cima de um bloco ou caixa.
Caso haja alguma caixa que não se encontre em cima de uma outra 'Caixa' ou 'Bloco' a função retorna o valor 'False' , caso não haja nenhuma caixa flutuante a função retorna o valor 'True'

Possui como funções auxiliares as funções: 'caixas' , 'verificarCaixas' .

== Exemplos de utilização:

>>> caixasFlutuantes [(Caixa,(1,1)),(Bloco,(1,2))]
True
>>> caixasFlutuantes [(Caixa,(1,1)),(Bloco,(1,3))]
False

== Propriedades:

prop> caixasFlutuantes [] = True
-}

caixasFlutuantes :: [(Peca, Coordenadas)] -> Bool
caixasFlutuantes [] = True
caixasFlutuantes l = verificarCaixas (caixas l) l



{- | A função 'caixas' recebe um mapa e devolve uma lista com as 'Coordenadas' de todas as caixas. 

== Exemplos de utilização:

>>> caixas [(Caixa,(3,2)),(Caixa,(3,1)),(Bloco,(3,3))]
[(3,2),(3,1)]

== Propriedades:

prop> caixas [] = []
-}

caixas :: [(Peca, Coordenadas)] -> [Coordenadas]
caixas [] = []
caixas ((p,(x,y)):xs)
    | p == Caixa = (x,y) : caixas xs
    | otherwise = caixas xs



{- | A função 'verificaCaixas' verifica coordenada a coodenada para uma dada lista de coordenadas e um mapa (sob a forma lista de pares) se essas mesmas caixas possuem em baixo de si uma outra 'Caixa'
ou 'Bloco' , caso haja alguma 'Caixa' que não cumpra essas condições a função retorna 'False'

Retorna verdadeiro caso todas as caixas se encontrem em cima de um bloco ou outra caixa.

== Exemplos de utilização:

>>> verificarCaixas [(1,2)] [(Bloco,(1,3)),(Caixa,(1,2))]
True

>>> verificarCaixas [(1,2)] [(Bloco,(1,4)),(Caixa,(1,2))]
False

== Propriedades:

prop> verificarCaixas [] _ = True
-}

verificarCaixas :: [Coordenadas] -> [(Peca, Coordenadas)] -> Bool
verificarCaixas [] _ = True
verificarCaixas ((a,b):xs) lista
    | (buscaTipo lista (a,b+1) == Caixa) || (buscaTipo lista (a,b+1) == Bloco) = verificarCaixas xs lista
    | otherwise = False



-----------------------------------------------------------------------------------------------------------
----------------------------------- (4.) Verificação de espaços vazios  -----------------------------------
-----------------------------------------------------------------------------------------------------------

{- | A função 'existirVazio' verifica para um dado mapa se este possui algum 'Vazio' , caso uma lista de pecas seja de igual tamanho ao produto das 'Coordenadas' a função retorna 'False'. 
Se for menor é porque existem espaços Vazios , retornando 'True'

Possui como funções auxiliares as funções: 'existirVazioOmissao' , 'existirVazioSemOmissao' , 'existirVazioAux'
== Exemplos de utilização:

>>> existirVazio [(Bloco,(1,1)),(Porta,(1,0))]
True

>>> existirVazio [(Porta,(0,0))]
False

== Propriedades:

prop> existirVazio [] = False
-}

existirVazio :: [(Peca, Coordenadas)] -> Bool
existirVazio l = existirVazioSemOmissao l || existirVazioOmissao l



{- | Função que verifica se existem Vazios por omissão, ou seja, caso uma lista de pecas seja de igual tamanho ao produto das maiores 'Coordenadas' retorna 'False' . Se for menor é porque existem espaços Vazios.

Utiliza como funções auxiliares: 'maximumX' e 'maximumY'

== Exemplo de utilização:

>>> existirVazioOmissao [(Bloco,(1,1)),(Porta,(1,0))]
True

>>> existirVazioOmissao [(Porta,(0,0))]
False

== Propriedades:

prop> existirVazioOmissao [] = False
-}

existirVazioOmissao :: [(Peca, Coordenadas)] -> Bool
existirVazioOmissao [] = False
existirVazioOmissao l = length l /= tamanho
    where tamanho = (maximumX l + 1) * (maximumY l + 1)



{- | Função que verifica se existem Vazios sem omissão, se for encontrado pelo menos um 'Vazio' a função retorna 'True'

Utiliza como função auxiliar: 'existirVazioAux'

== Exemplo de utilização:

>>> existirVazioSemOmissao [(Bloco,(1,1)),(Porta,(1,0))]
False

>>> existirVazioSemOmissao [(Vazio,(0,0))]
True

== Propriedades:

prop> existirVazioSemOmissao [] = False
-}

existirVazioSemOmissao :: [(Peca, Coordenadas)] -> Bool
existirVazioSemOmissao [] = False
existirVazioSemOmissao l = existirVazioAux l > 0



{- | Função auxiliar que conta o número de vazios existentes numa lista de pecas

== Exemplo de utilização:

>>> existirVazioAux [(Bloco,(1,1)),(Porta,(1,0))]
0

>>> existirVazioAux [(Vazio,(0,0))]
1

== Propriedades:

prop> existirVazioSemOmissao [] = 0
-}

existirVazioAux :: [(Peca, Coordenadas)] -> Int
existirVazioAux [] = 0
existirVazioAux ((a, xs):t)
    | a == Vazio = 1 + existirVazioAux t
    | otherwise = existirVazioAux t



-----------------------------------------------------------------------------------------------------------
---------------------------------------- (5.) Verificação do chão  ---------------------------------------- 
-----------------------------------------------------------------------------------------------------------

{- | Função que verifica se existe um chão contínuo sem blocos soltos em baixo.
Utiliza as funções 'existeChaoContinuo' e 'blocosIlha' -}

existeChao :: [(Peca, Coordenadas)] -> Bool
existeChao l = existeChaoContinuo l && blocosIlha l



{- | Função que verifica se existe um chão contínuo

Utiliza como funções auxiliares: 'existeChaoAux' , 'existeChaoAux2' e 'maximumX'

== Exemplo de utilização:

>>> existeChao [(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,1))]
True

>>> existeChao [(Bloco,(0,1)),(Bloco,(2,1))]
False
-}

existeChaoContinuo :: [(Peca, Coordenadas)] -> Bool
existeChaoContinuo l = existeChaoAux (maximumX l) (existeChaoAux2 l)



{- | Função que verifica se numa lista de Maybe Coordenadas existe uma Coordenada que contenha o elemento de maior X, que significa que a função 'listaContinua' encontrou um caminho contínuo até ao fim do mapa. -}

existeChaoAux :: Int -> [Maybe Coordenadas] -> Bool
existeChaoAux _ [] = False
existeChaoAux a (Nothing:t) = existeChaoAux a t
existeChaoAux a (Just (x,y):t)
    | x == a = True
    | otherwise = existeChaoAux a t



{- | Função que cria uma lista que representa um caminho contínuo no mapa.
Utiliza as funções auxiliares 'listaContinua', 'removeBlocosX0', 'listaBlocosMaybe', 'ultimoBloco', 'listaBlocos', 'maximumY' e 'deleteTecto'
-}

existeChaoAux2 :: [(Peca, Coordenadas)] -> [Maybe Coordenadas]
existeChaoAux2 l = listaContinua (removeBlocosX0 (listaBlocosMaybe (deleteTecto l))) (ultimoBloco (listaBlocos l) 0 (maximumY l))



{- | Função que cria uma lista de coordenadas, que representam um caminho continuo no mapa: se a lista incluir um elemento cuja fst coordenada (x) seja igual ao maior x da lista original então pode-se concluir que a função encontrou um caminho contínuo (o que será verificado com a função 'existeChaoAux').

Comecemos por procurar um caminho à direita, se apenas existir um bloco em (x+1,y) então criamos uma lista com esse bloco e chamamos de novo a função que vai adicionando blocos conectados.

Se existirem mais do que um bloco conectados, por exemplo à direita e em cima, então chamamos a função duas vezes para criar uma lista que inclua esses dois possiveis caminhos.

Caso nao exista nenhum bloco à direita\Nordeste\Sudeste e só exista para cima e\ou para baixo, não precisamos de  adicionar esse novo bloco à lista pois o objectivo da função é chegar a um elemento com o valor máximo da coordenada x e ao andar para cima continuamos na mesma linha vertical, ou seja, o x não muda.

Utiliza a função auxiliar 'delete'

== Exemplo de utilização:
>>> listaContinua [Just (0,1),Just (1,1),Just (2,1),Just (3,1)] (Just (0,1))
[Just (1,1),Just (2,1),Just (3,1)]

>>> listaContinua [Just (0,1),Just (1,1),Just (3,1)] (Just (0,1))
[Just (1,1)]
-}

listaContinua :: [Maybe Coordenadas] -> Maybe Coordenadas -> [Maybe Coordenadas]
listaContinua _ Nothing = []
listaContinua l (Just (x,y))
    | blocoDireita              = Just (x+1, y)   : listaContinua l0 (Just (x+1,y))
    | blocoDireitaCima          = Just (x+1, y)   : listaContinua l0 (Just (x+1,y))    ++ listaContinua l3 (Just (x,y-1))
    | blocoDireitaBaixo         = Just (x+1, y)   : listaContinua l0 (Just (x+1,y))    ++ listaContinua l4 (Just (x,y+1))
    | blocoDireitaCimaBaixo     = Just (x+1, y)   : listaContinua l0 (Just (x+1,y))    ++ listaContinua l3 (Just (x,y-1)) ++ listaContinua l4 (Just (x,y+1))
    | blocoNordeste             = Just (x+1, y-1) : listaContinua l1 (Just (x+1,y-1))
    | blocoNordesteCima         = Just (x+1, y-1) : listaContinua l1 (Just (x+1,y-1))  ++ listaContinua l3 (Just (x,y-1))
    | blocoNordesteBaixo        = Just (x+1, y-1) : listaContinua l1 (Just (x+1,y-1))  ++ listaContinua l4 (Just (x,y+1))
    | blocoNordesteCimaBaixo    = Just (x+1, y-1) : listaContinua l1 (Just (x+1,y-1))  ++ listaContinua l3 (Just (x,y-1)) ++ listaContinua l4 (Just (x,y+1))
    | blocoSudeste              = Just (x+1, y+1) : listaContinua l2 (Just (x+1,y+1))
    | blocoSudesteCima          = Just (x+1, y+1) : listaContinua l2 (Just (x+1,y+1))  ++ listaContinua l3 (Just (x,y-1))
    | blocoSudesteBaixo         = Just (x+1, y+1) : listaContinua l2 (Just (x+1,y+1))  ++ listaContinua l4 (Just (x,y+1))
    | blocoSudesteCimaBaixo     = Just (x+1, y+1) : listaContinua l2 (Just (x+1,y+1))  ++ listaContinua l3 (Just (x,y-1)) ++ listaContinua l4 (Just (x,y+1))
    | blocoCima                 =                   listaContinua l3 (Just (x,y-1))
    | blocoBaixo                =                   listaContinua l4 (Just (x,y+1))
    | blocoCimaBaixo            =                   listaContinua l3 (Just (x,y-1))    ++ listaContinua l4 (Just (x,y+1))
    | otherwise                 =                   []
    where l0 = delete (Just (x+1,y)) l
          l1 = delete (Just (x+1,y-1)) l
          l2 = delete (Just (x+1,y+1)) l
          l3 = delete (Just (x,y-1)) l
          l4 = delete (Just (x,y+1)) l
          blocoDireita              = Just (x+1, y)   `elem` l  &&  Just (x, y+1) `notElem` l  &&  Just (x, y-1) `notElem` l
          blocoDireitaCima          = Just (x+1, y)   `elem` l  &&  Just (x, y-1) `elem`    l  &&  Just (x, y+1) `notElem` l
          blocoDireitaBaixo         = Just (x+1, y)   `elem` l  &&  Just (x, y+1) `elem`    l  &&  Just (x, y-1) `notElem` l
          blocoDireitaCimaBaixo     = Just (x+1, y)   `elem` l  &&  Just (x, y+1) `elem`    l  &&  Just (x, y-1) `elem`    l
          blocoNordeste             = Just (x+1, y-1) `elem` l  &&  Just (x, y+1) `notElem` l  &&  Just (x, y-1) `notElem` l
          blocoNordesteCima         = Just (x+1, y-1) `elem` l  &&  Just (x, y-1) `elem`    l  &&  Just (x, y+1) `notElem` l
          blocoNordesteBaixo        = Just (x+1, y-1) `elem` l  &&  Just (x, y+1) `elem`    l  &&  Just (x, y-1) `notElem` l
          blocoNordesteCimaBaixo    = Just (x+1, y-1) `elem` l  &&  Just (x, y+1) `elem`    l  &&  Just (x, y-1) `elem`    l
          blocoSudeste              = Just (x+1, y+1) `elem` l  &&  Just (x, y+1) `notElem` l  &&  Just (x, y-1) `notElem` l
          blocoSudesteCima          = Just (x+1, y+1) `elem` l  &&  Just (x, y-1) `elem`    l  &&  Just (x, y+1) `notElem` l
          blocoSudesteBaixo         = Just (x+1, y+1) `elem` l  &&  Just (x, y+1) `elem`    l  &&  Just (x, y-1) `notElem` l
          blocoSudesteCimaBaixo     = Just (x+1, y+1) `elem` l  &&  Just (x, y+1) `elem`    l  &&  Just (x, y-1) `elem`    l
          blocoCima                 = Just (x, y-1)   `elem` l  &&  Just (x, y+1) `notElem` l
          blocoBaixo                = Just (x, y+1)   `elem` l  &&  Just (x, y-1) `notElem` l
          blocoCimaBaixo            = Just (x, y+1)   `elem` l  &&  Just (x, y-1) `elem`    l



{- | Função que cria uma lista das Coordenadas de todos os blocos de uma lista de Pecas -}
listaBlocos :: [(Peca, Coordenadas)] -> [Coordenadas]
listaBlocos [] = []
listaBlocos ((p, (x, y)):t)
    | p == Bloco = (x, y) : listaBlocos t
    | otherwise = listaBlocos t



{- | Função que cria uma lista de [Maybe Coordenadas] de todos os blocos de uma lista de Pecas. Utiliza a função 'listaBlocos' -}
listaBlocosMaybe :: [(Peca, Coordenadas)] -> [Maybe Coordenadas]
listaBlocosMaybe l = map Just (listaBlocos l)



{- | Função que retorna as coordenadas do último Bloco de uma dada linha vertical do mapa, sob a forma de (Just (x,y)) ou Nothing, caso não exista -}
ultimoBloco :: [Coordenadas] -> Int -> Int -> Maybe Coordenadas
ultimoBloco [] _ _ = Nothing
ultimoBloco l x y
    | y < 0 = Nothing
    | (x,y) `elem` l = Just (x,y)
    | otherwise = ultimoBloco l x (y-1)



{- | Função que que lista todas as coordenadas dos últimos Blocos de uma lista de pecas. Utiliza a função auxiliar 'listaUltimosBlocosAux' -}
listaUltimosBlocos :: [(Peca, Coordenadas)] -> [Maybe Coordenadas]
listaUltimosBlocos l = listaUltimosBlocosAux l 0 (maximumY l)



{- | Função que que lista as 'Coordenadas' dos últimos Blocos de uma lista de pecas, começando por um dado valor para a coordenada x -}
listaUltimosBlocosAux :: [(Peca, Coordenadas)] -> Int -> Int -> [Maybe Coordenadas]
listaUltimosBlocosAux [] _ _ = []
listaUltimosBlocosAux l x y
    | x == maxX+1 = []
    | otherwise = ultimoBloco (listaBlocos l) x y : listaUltimosBlocosAux l (x+1) y
    where maxX = maximumX l



{- | Função que que lista as coordenadas de todos os blocos conectados a um dado bloco. -}
blocosConectados :: [Maybe Coordenadas] -> Maybe Coordenadas -> [Maybe Coordenadas]
blocosConectados [] _ = []
blocosConectados _ Nothing = []
blocosConectados (Nothing:t) (Just (x,y)) = blocosConectados t (Just (x,y))
blocosConectados (Just (a,b):t) (Just (x,y))
    | x-1 <= a && a <= x+1 && y-1 <=b && b <= y+1 = Just (a,b) : blocosConectados t (Just (x,y))
    | otherwise = blocosConectados t (Just (x,y))



{- | Função que remove os Blocos que vão dar ao tecto: se existir um tecto continuo que esteja conectado a um chao não contínuo, a função listaContinua dará True ao invés de False, pelo que é preciso remover os blocos que vão dar ao tecto, quando um caminho é assegurado. -}
removeBlocosX0 :: [Maybe Coordenadas] -> [Maybe Coordenadas]
removeBlocosX0 [] = []
removeBlocosX0 (Nothing:t) = removeBlocosX0 t
removeBlocosX0 ((Just (x,y)):t)
    | x /= 0 = Just (x,y) : removeBlocosX0 t
    | otherwise = removeBlocosX0 t


{- | Função auxiliar que remove o tecto de uma mapa, pois este não é importante para verificar se o chão é contínuo.-}
deleteTecto :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
deleteTecto [] = []
deleteTecto ((p, (x,y)):t)
    | y /= 0 = (p, (x,y)) : deleteTecto t
    | otherwise = deleteTecto t



{- | Função que verifica se existem blocos soltos em baixo do mapa
Utiliza as funções 'contaBlocosIlha', 'blocosIlhaAux', 'listaBlocosMaybe' e 'listaUltimosBlocos'-}
blocosIlha :: [(Peca, Coordenadas)] -> Bool
blocosIlha l = contaBlocosIlha (blocosIlhaAux (listaBlocosMaybe l) (listaUltimosBlocos l)) == 0



{- | Função auxiliar que cria uma lista de listas de blocos conectados-}
blocosIlhaAux :: [Maybe Coordenadas] -> [Maybe Coordenadas] -> [[Maybe Coordenadas]]
blocosIlhaAux l = map (blocosConectados l)



{- | Função auxiliar que conta todas as listas que só têm um elemento, o que significa que são blocos que não estão connectados.
Se o número retornado for maior do que 0, significa que existem blocos soltos.-}
contaBlocosIlha :: [[Maybe Coordenadas]] -> Int
contaBlocosIlha [] = 0
contaBlocosIlha (h:t)
    | length h == 1 = 1 + contaBlocosIlha t
    | otherwise = contaBlocosIlha t