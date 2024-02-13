{- |
Module      : Tarefa6_2021li1g074
Description : Resolução de um puzzle
Copyright   : Miguel Ângelo Martins Guimarães;
            : Filipe Prudêncio Pacheco dos Santos;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.

O objectivo desta tarefa é implementar a função 'resolveJogo' que tenta resolver um jogo num número máximo de movimentos.
Resolver um jogo consisten em encontrar uma sequência de movimentos que o jogador pode realizar para chegar à porta.
Utiliza as funções : 'geraMovimentosFinal', 'listaCaixas', 'desconstroiMapa' e 'buscaPorta'

Como funciona :

Quando o limite de movimentos imposto é 0 (zero), a função resume-se a testar se o jogo já se encontra resolvido, utilizando a função auxiliar 'gameOver' definida no ficheiro FuncoesAuxiliares.hs

Quando este é superior a zero então:

- Primeiro verifica-se se a função 'geraMovimentos0' encontra uma lista que resolva o jogo, esta cria uma lista de movimentos onde o jogador não interage com nenhuma caixa.

- Seguidamente utiliza-se a função 'geraListaMovimentos1', que aplica a 'geraMovimentos1' várias vezes, uma para cada caixa que exista no jogo, gerando uma lista de listas de movimentos. Filtram-se as listas que vencem o jogo e seleciona-se a mais curta com a função 'selecionaMenor'.

- Caso os casos em cima falhem, utiliza-se a 'geraMovimentos2' que pode interagir com todas as caixas do jogo, criando uma lista de movimentos. Caso estes resolvam o jogo a função retorna Just [Movimento], caso contrário retorna Nothing.


As funções geraMovimentos funcionam de forma semelhante, alterando o número de caixas com que interagem. Estas seguem a seguinte estratégia:

A função começa por tentar encontrar um caminho até a porta sem interagir com caixas, se a porta estiver à esquerda do jogador, por exemplo, este anda para a esquerda ou trepa consoante o caminho se encontra vazio ou obstruído.
Se isto não for suficiente então o jogador encontra um caminho até à caixa mais próxima (que é dada pela função 'caixaMaisPerto') da mesma maneira que encontra um caminho para a porta abordado na condição anterior. Quando este chega à caixa interage com ela e começa mais uma vez a deslocar-se até a porta, largando a caixa quando encontra um obstáculo.
Depois de largar a caixa verifica-se se é possivel resolver o jogo sem interagir com mais nenhuma caixa, se for então adicionam-se esses movimentos à lista, caso contrário encontra-se a próxima caixa mais próxima e repete-se o ponto anterior.
As condições que ditam que movimento pode ser aplicado pelo jogador foram definidas utilizando a função 'buscaTipoMapa'

Dificuldades sentidas : A função funciona para mapas mais lineares mas caso seja necessário fazer algo mais complexo para resolver o jogo esta irá retornar Nothing. Exemplo: uma mapa onde a porta se encontre numa plataforma e é necessário criar uma escada de caixas para lá chegar. A função também não irá funcionar se for necessário pegar na mesma caixa mais do que uma vez.

No fim deste ficheiro encontram-se em comentário umas tentativas anteriores de resolução desta Tarefa. Inicialmente tentou-se resolver o jogo utilizando a função replicateM do Control.Monad, esta gera todas as listas de movimentos possiveis para uma dada length das quais se verifica se alguma vence o jogo, mas quando o limite de movimentos chega às dezenas esta demora muito tempo a correr pelo que não será viavel para resolver a Tarefa. Também em comentário encontra-se a função bruteForce, funciona de forma semelhante ao replicateM e por isso partilha os mesmos problemas.
-}

module Tarefa6_2021li1g074 where


--import Control.Monad (replicateM)


-- Import das funções essenciais
import LI12122
import FuncoesAuxiliares

import Tarefa2_2021li1g074
import Tarefa4_2021li1g074


-- | Função que tenta resolver um jogo num número máximo de movimentos, retornando Just [Movimento] ou Nothing caso tal não seja possivel
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo n jogo@(Jogo mapa (Jogador (a,b) dir val)) = geraMovimentosFinal jogo (x,y) n listCaixas
    where (x,y) = buscaPorta mapa
          listCaixas = listaCaixas (desconstroiMapa mapa)


-- | Função que seleciona uma função auxiliar para resolver um jogo, consoante o limite de movimentos imposto.
geraMovimentosFinal :: Jogo -> Coordenadas -> Int -> [Coordenadas] -> Maybe [Movimento]
geraMovimentosFinal jogo@(Jogo mapa (Jogador (a,b) dir val)) (x,y) n []
    | n == 0  = if gameOver jogo then Just [] else Nothing  -- jogo resolvido
    | caixas0 = Just lista0  -- zero caixas, como a lista de caixas esta fazia é o unico caso
    | otherwise = Nothing
    where lista0  = geraMovimentos0 jogo (x,y) n
          caixas0 = gameOver (correrMovimentos jogo lista0)

geraMovimentosFinal jogo@(Jogo mapa (Jogador (a,b) dir val)) (x,y) n (h:t)
    | n == 0  = if gameOver jogo then Just [] else Nothing  -- jogo resolvido
    | caixas0 = Just lista0  -- zero caixas
    | caixas1 = Just (selecionaMenor listas1)  -- uma caixa
    | caixas2 = Just lista2
    | otherwise = Nothing
    where -- listas de movimentos
          lista0  = geraMovimentos0 jogo (x,y) n
          listas1 = geraListaMovimentos1 jogo (x,y) h n False (h:t)
          lista2  = geraMovimentos2 jogo (x,y) cmp n False l2
          -- condições
          caixas0 = gameOver (correrMovimentos jogo lista0)
          caixas1 = not (null listas1)
          caixas2 = gameOver (correrMovimentos jogo lista2)
          -- auxiliar
          cmp = caixaMaisPerto jogo (h:t)
          l2  = delete cmp (h:t)

-- | Função auxiliar que gera uma lista de listas de movimentos, que correspondem a aplicar a função geraMovimentos1 a todas as caixas
geraListaMovimentos1 :: Jogo -> Coordenadas -> Coordenadas -> Int -> Bool -> [Coordenadas] -> [[Movimento]]
geraListaMovimentos1 jogo@(Jogo mapa (Jogador (a,b) dir v)) (x,y) (c1,c2) n val [] = []
geraListaMovimentos1 jogo@(Jogo mapa (Jogador (a,b) dir v)) (x,y) (c1,c2) n val (h:t)
    | gameOver (correrMovimentos jogo (geraMovimentos1 jogo (x,y) h n False)) = geraMovimentos1 jogo (x,y) h n False : geraListaMovimentos1 jogo (x,y) (c1,c2) n val t
    | otherwise = geraListaMovimentos1 jogo (x,y) (c1,c2) n val t

-- | Função que dada uma lista de listas de movimentos retorna a lista não vazia mais pequena
selecionaMenor :: [[Movimento]] -> [Movimento]
selecionaMenor l = snd $ minimum $ filter aux $ map (\x -> (length x, x)) l
    where aux (x,y) = x /= 0

-- | Função que devolve a coordenada da caixa mais perto do jogador, relativamente ao eixo dos x, caso existam várias devolve a de maior Y. Se estiverem empilhadas devolve a de cima.
caixaMaisPerto :: Jogo -> [Coordenadas] -> Coordenadas
caixaMaisPerto jogo@(Jogo mapa (Jogador (a,b) dir val)) l
    | esquerdaCaixas = (c-1,d-1) 
    | direitaCaixas  = (c+1,d-1) 
    | otherwise      = (c,d)
    where aux (x,y) = x == caixaMaisPertoAux jogo l
          (c,d) = snd $ maximum $ map (\(x,y) -> (y,(x,y))) $ filter aux l
          esquerdaCaixas = buscaTipoMapa mapa (c-1,d) == Just Caixa && buscaTipoMapa mapa (c-1,d-1) == Just Caixa
          direitaCaixas  = buscaTipoMapa mapa (c+1,d) == Just Caixa && buscaTipoMapa mapa (c+1,d-1) == Just Caixa

-- | Função auxiliar que devolve o valor do eixo X da caixa mais próxima ao jogador
caixaMaisPertoAux :: Jogo -> [Coordenadas] -> Int
caixaMaisPertoAux (Jogo mapa (Jogador (a,b) dir val)) l = fst $ snd $ minimum $ map (\(x,y) -> (abs (x-a),(x,y))) l

----------------------------------------- 0 caixas

{- | Função que gera uma lista de movimentos quando não interagindo com caixas no jogo
Recebe as coordenadas da porta, o número maximo de movimentos e retorna uma lista de movimentos.
-}
geraMovimentos0 :: Jogo -> Coordenadas -> Int -> [Movimento]
geraMovimentos0 jogo@(Jogo mapa (Jogador (x,y) dir val)) (a,b) n -- (a,b) = porta, n = número de movimentos
    | n == 0    = []
    | fimDeJogo = []
    | vaiEsquerda = AndarEsquerda : geraMovimentos0 (moveJogador jogo AndarEsquerda) (a,b) (n-1)
    | vaiDireita  = AndarDireita  : geraMovimentos0 (moveJogador jogo AndarDireita)  (a,b) (n-1)
    | vaiTrepar   = Trepar        : geraMovimentos0 (moveJogador jogo Trepar)        (a,b) (n-1)
    | otherwise = []
    where fimDeJogo = (a,b) == (x,y)
          -- aplicar movimento
          vaiEsquerda = esquerdaVazioFalse || esquerdaBuracoFalse || esquerdaVazioTrue || esquerdaBuracoTrue || esquerdaTreparFalseEste || esquerdaTreparTrueEste
          vaiDireita  = direitaVazioFalse  || direitaBuracoFalse  || direitaVazioTrue  || direitaBuracoTrue  || direitaTreparFalseOeste || direitaTreparTrueOeste
          vaiTrepar   = esquerdaTreparFalseOeste || esquerdaTreparTrueOeste || direitaTreparFalseEste || direitaTreparTrueEste
          -----------------  andar esquerda (Oeste)
          esquerdaVazioFalse  =  not val && a < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta
          esquerdaBuracoFalse =  not val && a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta)
          ----
          esquerdaVazioTrue  = val && a < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          esquerdaBuracoTrue = val && a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta) && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          ----------------- andar direita (Este)
          direitaVazioFalse  = not val && a > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta
          direitaBuracoFalse = not val && a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta)
          ----
          direitaVazioTrue  = val && a > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          direitaBuracoTrue = val && a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta) && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          -- trepar esquerda
          esquerdaTreparFalseOeste = not val && dir == Oeste && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparFalseEste  = not val && dir == Este  && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          ----
          esquerdaTreparTrueOeste = val && dir == Oeste && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparTrueEste  = val && dir == Este  && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          -- trepar direita
          direitaTreparFalseOeste = not val && dir == Oeste && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparFalseEste  = not val && dir == Este  && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----
          direitaTreparTrueOeste = val && dir == Oeste && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparTrueEste  = val && dir == Este  && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)



----------------------------------------- 1 caixas

-- | Função semelhante a 'geraMovimentos0', que gera uma lista de movimentos, interagindo apenas com uma caixa do jogo 
geraMovimentos1 :: Jogo -> Coordenadas -> Coordenadas -> Int -> Bool -> [Movimento]
geraMovimentos1 jogo@(Jogo mapa (Jogador (x,y) dir val)) (a,b) (c1,c2) n False -- quando bool final está False o jogador procura uma caixa, quando True procura a porta
    | n == 0    = []
    | fimDeJogo = []
    | vaiInterage = InterageCaixa : geraMovimentos1 (moveJogador jogo InterageCaixa) (a,b) (c1,c2) (n-1) True
    | vaiEsquerda = AndarEsquerda : geraMovimentos1 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) False
    | vaiDireita  = AndarDireita  : geraMovimentos1 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) False
    | vaiTrepar   = Trepar        : geraMovimentos1 (moveJogador jogo Trepar)        (a,b) (c1,c2) (n-1) False
    | c1 == x     = if buscaTipoMapa mapa (x-1,y) == Just Vazio && buscaTipoMapa mapa (x-1,y+1) == Just Vazio
                    then AndarEsquerda : geraMovimentos1 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) False
                    else AndarDireita  : geraMovimentos1 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) False
    | otherwise = []
    where fimDeJogo = (a,b) == (x,y)
          -- aplicar movimento
          vaiEsquerda = esquerdaVazioFalse || esquerdaBuracoFalse || esquerdaVazioTrue || esquerdaBuracoTrue || esquerdaTreparFalseEste || esquerdaTreparTrueEste
          vaiDireita  = direitaVazioFalse  || direitaBuracoFalse  || direitaVazioTrue  || direitaBuracoTrue  || direitaTreparFalseOeste || direitaTreparTrueOeste
          vaiTrepar   = esquerdaTreparFalseOeste || esquerdaTreparTrueOeste || direitaTreparFalseEste || direitaTreparTrueEste
          vaiInterage = esquerdaCaixaOeste || direitaCaixaEste
          -----------------  andar esquerda (Oeste)
          esquerdaVazioFalse  =  not val && c1 < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta
          esquerdaBuracoFalse =  not val && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta)
          ----
          esquerdaVazioTrue  = val && c1 < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          esquerdaBuracoTrue = val && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta) && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          ----------------- andar direita (Este)
          direitaVazioFalse  = not val && c1 > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta
          direitaBuracoFalse = not val && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta)
          ----
          direitaVazioTrue  = val && c1 > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          direitaBuracoTrue = val && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta) && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          -- trepar esquerda
          esquerdaTreparFalseOeste = not val && dir == Oeste && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparFalseEste  = not val && dir == Este  && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          ----
          esquerdaTreparTrueOeste = val && dir == Oeste && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparTrueEste  = val && dir == Este  && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          -- trepar direita
          direitaTreparFalseOeste = not val && dir == Oeste && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparFalseEste  = not val && dir == Este  && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----
          direitaTreparTrueOeste = val && dir == Oeste && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparTrueEste  = val && dir == Este  && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----------------- Interage Caixa False
          esquerdaCaixaOeste = not val && dir == Oeste && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Caixa  && (x-1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x-1,y-1) == Just Vazio
          esquerdaCaixaEste  = not val && dir == Este  && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Caixa  && (x-1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x-1,y-1) == Just Vazio
          ----
          direitaCaixaOeste = not val && dir == Oeste && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Caixa && (x+1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x+1,y-1) == Just Vazio
          direitaCaixaEste  = not val && dir == Este  && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Caixa && (x+1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x+1,y-1) == Just Vazio

geraMovimentos1 jogo@(Jogo mapa (Jogador (x,y) dir val)) (a,b) (c1,c2) n True -- quando bool final está False o jogador procura uma caixa, quando True procura a porta
    | n == 0    = []
    | fimDeJogo = []
    | not val   = geraMovimentos0 jogo (a,b) n
    | vaiInterage = InterageCaixa : geraMovimentos1 (moveJogador jogo InterageCaixa) (a,b) (c1,c2) (n-1) True
    | prioridadeE = AndarEsquerda : geraMovimentos1 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) True
    | prioridadeD = AndarDireita  : geraMovimentos1 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) True
    | vaiEsquerda = AndarEsquerda : geraMovimentos1 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) True
    | vaiDireita  = AndarDireita  : geraMovimentos1 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) True
    | vaiTrepar   = Trepar        : geraMovimentos1 (moveJogador jogo Trepar)        (a,b) (c1,c2) (n-1) True
    | otherwise = []
    where fimDeJogo = (a,b) == (x,y)
          -- aplicar movimento
          vaiEsquerda = esquerdaVazioFalse || esquerdaBuracoFalse || esquerdaVazioTrue || esquerdaBuracoTrue || esquerdaTreparFalseEste || esquerdaTreparTrueEste
          vaiDireita  = direitaVazioFalse  || direitaBuracoFalse  || direitaVazioTrue  || direitaBuracoTrue  || direitaTreparFalseOeste || direitaTreparTrueOeste
          vaiTrepar   = esquerdaTreparFalseOeste || esquerdaTreparTrueOeste || direitaTreparFalseEste || direitaTreparTrueEste
          vaiInterage = esquerdaDoisBlocosOeste  || direitaDoisBlocosEste
          prioridadeE = direitaDoisBlocosOeste
          prioridadeD = esquerdaDoisBlocosEste
          -----------------  andar esquerda (Oeste)
          esquerdaVazioFalse  =  not val && a < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta
          esquerdaBuracoFalse =  not val && a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta)
          ----
          esquerdaVazioTrue  = val && a < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          esquerdaBuracoTrue = val && a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta) && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          ----------------- andar direita (Este)
          direitaVazioFalse  = not val && a > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta
          direitaBuracoFalse = not val && a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta)
          ----
          direitaVazioTrue  = val && a > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          direitaBuracoTrue = val && a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta) && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          -- trepar esquerda
          esquerdaTreparFalseOeste = not val && dir == Oeste && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparFalseEste  = not val && dir == Este  && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          ----
          esquerdaTreparTrueOeste = val && dir == Oeste && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparTrueEste  = val && dir == Este  && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          -- trepar direita
          direitaTreparFalseOeste = not val && dir == Oeste && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparFalseEste  = not val && dir == Este  && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----
          direitaTreparTrueOeste = val && dir == Oeste && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparTrueEste  = val && dir == Este  && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----------------- Interage Caixa True
          -- largar caixa esquerda
          esquerdaDoisBlocosOeste = dir == Oeste && (bbEsquerda || bbbEsquerda || bbbEsquerda1c || bbbEsquerda2c)
          esquerdaDoisBlocosEste  = dir == Este  && (bbEsquerda || bbbEsquerda || bbbEsquerda1c || bbbEsquerda2c)
          -- largar caixa direita
          direitaDoisBlocosOeste = dir == Oeste && (bbDireita || bbbDireita || bbbDireita1c || bbbDireita2c)
          direitaDoisBlocosEste  = dir == Este  && (bbDireita || bbbDireita || bbbDireita1c || bbbDireita2c)
          -- obstaculos
          bbEsquerda = a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && buscaTipoMapa mapa (x-2,y) == Just Bloco && buscaTipoMapa mapa (x-2,y-1) == Just Bloco && buscaTipoMapa mapa (x-2,y-2) == Just Vazio
          bbDireita  = a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && buscaTipoMapa mapa (x+2,y) == Just Bloco && buscaTipoMapa mapa (x+2,y-1) == Just Bloco && buscaTipoMapa mapa (x+2,y-2) == Just Vazio
          ----
          bbbEsquerda   = a < x &&  buscaTipoMapa mapa (x-1,y) == Just Vazio &&  buscaTipoMapa mapa (x-2,y) == Just Bloco  && buscaTipoMapa mapa (x-2,y-1) == Just Bloco &&  buscaTipoMapa mapa (x-2,y-2) == Just Bloco
          bbbEsquerda1c = a < x && (buscaTipoMapa mapa (x-1,y) == Just Caixa ||  buscaTipoMapa mapa (x-1,y) == Just Bloco) && buscaTipoMapa mapa (x-1,y-1) == Just Vazio &&  buscaTipoMapa mapa (x-2, y)  == Just Bloco && buscaTipoMapa mapa (x-2,y-1) == Just Bloco  && buscaTipoMapa mapa (x-2,y-2) == Just Bloco
          bbbEsquerda2c = a < x &&  buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-2,y) == Just Caixa  || buscaTipoMapa mapa (x-2, y) == Just Bloco) && (buscaTipoMapa mapa (x-2,y-1) == Just Caixa || buscaTipoMapa mapa (x-2,y-1) == Just Bloco) && buscaTipoMapa mapa (x-3, y)  == Just Bloco && buscaTipoMapa mapa (x-3,y-1) == Just Bloco && buscaTipoMapa mapa (x-3,y-2) == Just Bloco
          ----
          bbbDireita  =  a > x &&  buscaTipoMapa mapa (x+1,y) == Just Vazio &&  buscaTipoMapa mapa (x+2,y) == Just Bloco  && buscaTipoMapa mapa (x+2,y-1) == Just Bloco &&  buscaTipoMapa mapa (x+2,y-2) == Just Bloco
          bbbDireita1c = a > x && (buscaTipoMapa mapa (x+1,y) == Just Caixa ||  buscaTipoMapa mapa (x+1,y) == Just Bloco) && buscaTipoMapa mapa (x+1,y-1) == Just Vazio &&  buscaTipoMapa mapa (x+2, y)  == Just Bloco && buscaTipoMapa mapa (x+2,y-1) == Just Bloco  && buscaTipoMapa mapa (x+2,y-2) == Just Bloco
          bbbDireita2c = a > x &&  buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+2,y) == Just Caixa  || buscaTipoMapa mapa (x+2, y) == Just Bloco) && (buscaTipoMapa mapa (x+2,y-1) == Just Caixa || buscaTipoMapa mapa (x+2,y-1) == Just Bloco) && buscaTipoMapa mapa (x+3, y)  == Just Bloco && buscaTipoMapa mapa (x+3,y-1) == Just Bloco && buscaTipoMapa mapa (x+3,y-2) == Just Bloco



----------------------------------------- várias caixas

-- | Função semelhante a 'geraMovimentos0' e 'geraMovimentos1', que gera uma lista de movimentos, interagindo com várias caixas do jogo 
geraMovimentos2 :: Jogo -> Coordenadas -> Coordenadas -> Int -> Bool -> [Coordenadas] -> [Movimento]
geraMovimentos2 jogo@(Jogo mapa (Jogador (x,y) dir val)) (a,b) (c1,c2) n False lista -- quando bool final está False o jogador procura uma caixa, quando True procura a porta
    | n == 0    = []
    | fimDeJogo = []
    | vaiInterage = InterageCaixa : geraMovimentos2 (moveJogador jogo InterageCaixa) (a,b) (c1,c2) (n-1) True  lista
    | vaiEsquerda = AndarEsquerda : geraMovimentos2 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) False lista
    | vaiDireita  = AndarDireita  : geraMovimentos2 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) False lista
    | vaiTrepar   = Trepar        : geraMovimentos2 (moveJogador jogo Trepar)        (a,b) (c1,c2) (n-1) False lista
    | c1 == x     = if buscaTipoMapa mapa (x-1,y) == Just Vazio && buscaTipoMapa mapa (x-1,y+1) == Just Vazio
                    then AndarEsquerda : geraMovimentos2 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) False lista
                    else AndarDireita  : geraMovimentos2 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) False lista
    | otherwise = []
    where fimDeJogo = (a,b) == (x,y)
          -- aplicar movimento
          vaiEsquerda = esquerdaVazioFalse || esquerdaBuracoFalse || esquerdaVazioTrue || esquerdaBuracoTrue || esquerdaTreparFalseEste || esquerdaTreparTrueEste
          vaiDireita  = direitaVazioFalse  || direitaBuracoFalse  || direitaVazioTrue  || direitaBuracoTrue  || direitaTreparFalseOeste || direitaTreparTrueOeste
          vaiTrepar   = esquerdaTreparFalseOeste || esquerdaTreparTrueOeste || direitaTreparFalseEste || direitaTreparTrueEste
          vaiInterage = esquerdaCaixaOeste || direitaCaixaEste
          -----------------  andar esquerda (Oeste)
          esquerdaVazioFalse  =  not val && c1 < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta
          esquerdaBuracoFalse =  not val && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta)
          ----
          esquerdaVazioTrue  = val && c1 < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          esquerdaBuracoTrue = val && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta) && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          ----------------- andar direita (Este)
          direitaVazioFalse  = not val && c1 > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta
          direitaBuracoFalse = not val && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta)
          ----
          direitaVazioTrue  = val && c1 > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          direitaBuracoTrue = val && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta) && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          -- trepar esquerda
          esquerdaTreparFalseOeste = not val && dir == Oeste && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparFalseEste  = not val && dir == Este  && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          ----
          esquerdaTreparTrueOeste = val && dir == Oeste && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparTrueEste  = val && dir == Este  && c1 < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          -- trepar direita
          direitaTreparFalseOeste = not val && dir == Oeste && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparFalseEste  = not val && dir == Este  && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----
          direitaTreparTrueOeste = val && dir == Oeste && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparTrueEste  = val && dir == Este  && c1 > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----------------- Interage Caixa False
          esquerdaCaixaOeste = not val && dir == Oeste && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Caixa  && (x-1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x-1,y-1) == Just Vazio
          esquerdaCaixaEste  = not val && dir == Este  && c1 < x && buscaTipoMapa mapa (x-1,y) == Just Caixa  && (x-1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x-1,y-1) == Just Vazio
          ----
          direitaCaixaOeste = not val && dir == Oeste && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Caixa && (x+1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x+1,y-1) == Just Vazio
          direitaCaixaEste  = not val && dir == Este  && c1 > x && buscaTipoMapa mapa (x+1,y) == Just Caixa && (x+1,y) == (c1,c2) && buscaTipoMapa mapa (x,y-1) == Just Vazio && buscaTipoMapa mapa (x+1,y-1) == Just Vazio

geraMovimentos2 jogo@(Jogo mapa (Jogador (x,y) dir val)) (a,b) (c1,c2) n True lista -- quando bool final está False o jogador procura uma caixa, quando True procura a porta
    | n == 0    = []
    | fimDeJogo = []
    | listaNula = if gOver then geraMovimentos0 jogo (a,b) n else []
    | not val   = if gOver then geraMovimentos0 jogo (a,b) n else geraMovimentos2 jogo (a,b) cmp n False (delete cmp lista)
    | vaiInterage = InterageCaixa : geraMovimentos2 (moveJogador jogo InterageCaixa) (a,b) (c1,c2) (n-1) True lista
    | prioridadeE = AndarEsquerda : geraMovimentos2 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) True lista
    | prioridadeD = AndarDireita  : geraMovimentos2 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) True lista
    | vaiEsquerda = AndarEsquerda : geraMovimentos2 (moveJogador jogo AndarEsquerda) (a,b) (c1,c2) (n-1) True lista
    | vaiDireita  = AndarDireita  : geraMovimentos2 (moveJogador jogo AndarDireita)  (a,b) (c1,c2) (n-1) True lista
    | vaiTrepar   = Trepar        : geraMovimentos2 (moveJogador jogo Trepar)        (a,b) (c1,c2) (n-1) True lista
    | otherwise = []
    where fimDeJogo = (a,b) == (x,y)
          listaNula = null lista && not val
          gOver = gameOver (correrMovimentos jogo (geraMovimentos0 jogo (a,b) n))
          cmp = caixaMaisPerto jogo lista
          -- aplicar movimento
          vaiEsquerda = esquerdaVazioFalse || esquerdaBuracoFalse || esquerdaVazioTrue || esquerdaBuracoTrue || esquerdaTreparFalseEste || esquerdaTreparTrueEste
          vaiDireita  = direitaVazioFalse  || direitaBuracoFalse  || direitaVazioTrue  || direitaBuracoTrue  || direitaTreparFalseOeste || direitaTreparTrueOeste
          vaiTrepar   = esquerdaTreparFalseOeste || esquerdaTreparTrueOeste || direitaTreparFalseEste || direitaTreparTrueEste
          vaiInterage = esquerdaDoisBlocosOeste  || direitaDoisBlocosEste
          prioridadeE = direitaDoisBlocosOeste
          prioridadeD = esquerdaDoisBlocosEste
          -----------------  andar esquerda (Oeste)
          esquerdaVazioFalse  =  not val && a < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta
          esquerdaBuracoFalse =  not val && a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta)
          ----
          esquerdaVazioTrue  = val && a < x && (buscaTipoMapa mapa (x-1,y) == Just Vazio || buscaTipoMapa mapa (x-1, y) == Just Porta) && buscaTipoMapa mapa (x-1,y+1) /= Just Vazio && buscaTipoMapa mapa (x-1,y+1) /= Just Porta && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          esquerdaBuracoTrue = val && a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-1,y+1) == Just Vazio || buscaTipoMapa mapa (x-1,y+1) == Just Porta) && buscaTipoMapa mapa (x-1, y-1) == Just Vazio
          ----------------- andar direita (Este)
          direitaVazioFalse  = not val && a > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta
          direitaBuracoFalse = not val && a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta)
          ----
          direitaVazioTrue  = val && a > x && (buscaTipoMapa mapa (x+1,y) == Just Vazio || buscaTipoMapa mapa (x+1, y) == Just Porta) && buscaTipoMapa mapa (x+1,y+1) /= Just Vazio && buscaTipoMapa mapa (x+1,y+1) /= Just Porta && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          direitaBuracoTrue = val && a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+1,y+1) == Just Vazio || buscaTipoMapa mapa (x+1,y+1) == Just Porta) && buscaTipoMapa mapa (x+1, y-1) == Just Vazio
          -- trepar esquerda
          esquerdaTreparFalseOeste = not val && dir == Oeste && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparFalseEste  = not val && dir == Este  && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          ----
          esquerdaTreparTrueOeste = val && dir == Oeste && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          esquerdaTreparTrueEste  = val && dir == Este  && a < x && (buscaTipoMapa mapa (x-1,y-1) == Just Vazio || buscaTipoMapa mapa (x-1, y-1) == Just Porta) && buscaTipoMapa mapa (x-1, y-2) == Just Vazio && (buscaTipoMapa mapa (x-1,y) == Just Caixa || buscaTipoMapa mapa (x-1,y) == Just Bloco)
          -- trepar direita
          direitaTreparFalseOeste = not val && dir == Oeste && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparFalseEste  = not val && dir == Este  && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----
          direitaTreparTrueOeste = val && dir == Oeste && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          direitaTreparTrueEste  = val && dir == Este  && a > x && (buscaTipoMapa mapa (x+1,y-1) == Just Vazio || buscaTipoMapa mapa (x+1, y-1) == Just Porta) && buscaTipoMapa mapa (x+1, y-2) == Just Vazio && (buscaTipoMapa mapa (x+1,y) == Just Caixa || buscaTipoMapa mapa (x+1,y) == Just Bloco)
          ----------------- Interage Caixa True
          -- largar caixa esquerda
          esquerdaDoisBlocosOeste = dir == Oeste && (bbEsquerda || bbbEsquerda || bbbEsquerda1c || bbbEsquerda2c)
          esquerdaDoisBlocosEste  = dir == Este  && (bbEsquerda || bbbEsquerda || bbbEsquerda1c || bbbEsquerda2c)
          -- largar caixa direita
          direitaDoisBlocosOeste = dir == Oeste && (bbDireita || bbbDireita || bbbDireita1c || bbbDireita2c)
          direitaDoisBlocosEste  = dir == Este  && (bbDireita || bbbDireita || bbbDireita1c || bbbDireita2c)
          -- obstaculos
          bbEsquerda = a < x && buscaTipoMapa mapa (x-1,y) == Just Vazio && buscaTipoMapa mapa (x-2,y) == Just Bloco && buscaTipoMapa mapa (x-2,y-1) == Just Bloco && buscaTipoMapa mapa (x-2,y-2) == Just Vazio
          bbDireita  = a > x && buscaTipoMapa mapa (x+1,y) == Just Vazio && buscaTipoMapa mapa (x+2,y) == Just Bloco && buscaTipoMapa mapa (x+2,y-1) == Just Bloco && buscaTipoMapa mapa (x+2,y-2) == Just Vazio
          ----
          bbbEsquerda   = a < x &&  buscaTipoMapa mapa (x-1,y) == Just Vazio &&  buscaTipoMapa mapa (x-2,y) == Just Bloco  && buscaTipoMapa mapa (x-2,y-1) == Just Bloco &&  buscaTipoMapa mapa (x-2,y-2) == Just Bloco
          bbbEsquerda1c = a < x && (buscaTipoMapa mapa (x-1,y) == Just Caixa ||  buscaTipoMapa mapa (x-1,y) == Just Bloco) && buscaTipoMapa mapa (x-1,y-1) == Just Vazio &&  buscaTipoMapa mapa (x-2, y)  == Just Bloco && buscaTipoMapa mapa (x-2,y-1) == Just Bloco  && buscaTipoMapa mapa (x-2,y-2) == Just Bloco
          bbbEsquerda2c = a < x &&  buscaTipoMapa mapa (x-1,y) == Just Vazio && (buscaTipoMapa mapa (x-2,y) == Just Caixa  || buscaTipoMapa mapa (x-2, y) == Just Bloco) && (buscaTipoMapa mapa (x-2,y-1) == Just Caixa || buscaTipoMapa mapa (x-2,y-1) == Just Bloco) && buscaTipoMapa mapa (x-3, y)  == Just Bloco && buscaTipoMapa mapa (x-3,y-1) == Just Bloco && buscaTipoMapa mapa (x-3,y-2) == Just Bloco
          ----
          bbbDireita  =  a > x &&  buscaTipoMapa mapa (x+1,y) == Just Vazio &&  buscaTipoMapa mapa (x+2,y) == Just Bloco  && buscaTipoMapa mapa (x+2,y-1) == Just Bloco &&  buscaTipoMapa mapa (x+2,y-2) == Just Bloco
          bbbDireita1c = a > x && (buscaTipoMapa mapa (x+1,y) == Just Caixa ||  buscaTipoMapa mapa (x+1,y) == Just Bloco) && buscaTipoMapa mapa (x+1,y-1) == Just Vazio &&  buscaTipoMapa mapa (x+2, y)  == Just Bloco && buscaTipoMapa mapa (x+2,y-1) == Just Bloco  && buscaTipoMapa mapa (x+2,y-2) == Just Bloco
          bbbDireita2c = a > x &&  buscaTipoMapa mapa (x+1,y) == Just Vazio && (buscaTipoMapa mapa (x+2,y) == Just Caixa  || buscaTipoMapa mapa (x+2, y) == Just Bloco) && (buscaTipoMapa mapa (x+2,y-1) == Just Caixa || buscaTipoMapa mapa (x+2,y-1) == Just Bloco) && buscaTipoMapa mapa (x+3, y)  == Just Bloco && buscaTipoMapa mapa (x+3,y-1) == Just Bloco && buscaTipoMapa mapa (x+3,y-2) == Just Bloco

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------- Antigo -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- {-| Função principal determina os movimentos que vencem o jogo -}
-- resolveJogo :: Int -> Jogo -> Maybe [Movimento]
-- resolveJogo val jogo = bruteForce val jogo


-- -- Avalia se o numero de passos maximo é suficiente para chegar á porta, se for então procura a sequencia de movimentos que ganham o jogo
-- bruteForce :: Int -> Jogo -> Maybe [Movimento]
-- bruteForce val jogo 
--     | (val - distanciaPorta jogo) < 0 = Nothing
--     | otherwise = movimentosWin jogo ( base ++ looper )
--     where looper = loopMov (val-1) [[AndarEsquerda],[AndarDireita],[Trepar],[InterageCaixa]]
--           base = [[AndarEsquerda],[AndarDireita],[Trepar],[InterageCaixa]]

-- --------------------------- Criar Base / Árvores de movimentos ---------------------------


-- -- Executa um determinado número de vezes a função addMov criando uma lista semelhante a uma árvore 
-- loopMov :: Int -> [[Movimento]] -> [[Movimento]]
-- loopMov _ [] = []
-- loopMov 0 lista = []
-- loopMov val lista = addMov lista ++ loopMov (val-1) (addMov lista)

-- -- Recebe uma lista contendo listas de movimentos, clona cada uma destas listas 4 vezes e adiciona os movimentos (Esquerda,Direita,Trepar,Interagir) a cada uma 
-- addMov :: [[Movimento]] -> [[Movimento]]
-- addMov [] = []
-- addMov (x:xs) = [x++[AndarEsquerda]] ++ [x++[AndarDireita]] ++ [x++[Trepar]] ++ [x++[InterageCaixa]] ++ addMov xs
--     where tudo = x:xs

-- -- Igual ao loopMov mas apenas devolve a lista final ignorando as listas intermédias
-- loopCut :: Int -> [[Movimento]] -> [[Movimento]]
-- loopCut _ [] = []
-- loopCut dist lista 
--     | dist > 2 = loopCut (dist-1) (addMov lista)
--     | otherwise = if dist == 2 then addMov lista
--                   else []


-- --------------------------- Verificar se o jogo está ganho --------------------------- 


-- {-| Verifica se uma das listas de movimentos de uma lista que contém estas vence o jogo , caso positivo devolve a lista de movimentos que ganha o jogo -}
-- movimentosWin :: Jogo -> [[Movimento]] -> Maybe [Movimento]
-- movimentosWin _ [] = Nothing
-- movimentosWin jogo (x:xs)
--     | gameOver (correrMovimentos jogo x) = Just x
--     | otherwise = movimentosWin jogo xs 

-- {-| Verifica se um jogo está ganho-}
-- gameOver :: Jogo -> Bool
-- gameOver (Jogo mapa (Jogador (a,b) dir val)) = gameOverAux (buscaPorta (desconstroiMapa mapa)) (Jogo mapa (Jogador (a,b) dir val))

-- gameOverAux :: Coordenadas -> Jogo -> Bool
-- gameOverAux (x,y) (Jogo mapa (Jogador (a,b) dir val)) = (x,y) == (a,b)

-- buscaPorta :: [(Peca, Coordenadas)] -> Coordenadas
-- buscaPorta [] = error "Mapa inválido, não existe porta"
-- buscaPorta ((p, (x,y)):t)
--     | p == Porta = (x,y)
--     | otherwise = buscaPorta t 


-- --------------------------- replicateM e afins --------------------------- 

-- resolveJogo :: Int -> Jogo -> Maybe [Movimento]
-- resolveJogo n jogo = resolveAux n jogo (contaCaixas jogo)

-- resolveAux :: Int -> Jogo -> Int -> Maybe [Movimento]
-- resolveAux n jogo a
--     | n == 0 &&      gameOver jogo    = Just []  -- testar se o mapa já esta resolvido
--     | n == 0 && not (gameOver jogo)   = Nothing  -- testar se o mapa já esta resolvido
--     | n > 0  && (n - distanciaPorta jogo) < 0 = Nothing  -- ve se o jogo nao pode ser resolvido com dado n
--     | n > 0  && a == 0 && n <= 9 = replicateAux jogo (replicateM n movimentos0)  -- brute force quando nao ha caixas e menos de 9 movimentos, mais demora muito tempo (substituir por bruteForce)
--     | n > 0  && a == 0 = if gameOver (correrMovimentos jogo (botSemCaixas jogo)) then Just (botSemCaixas jogo) else Nothing -- para quando há mais do que 9 movimentos, usar botSemCaixas
--     | n > 0  && a > 0 && n <= 7 = replicateAux jogo (replicateM n movimentos1)   -- brute force quando ha caixas e menos de 7 movimentos, (substituir por bruteForce)
--     | otherwise = Nothing -- otherwise ainda não definido, por enquando Nothing. Fazer bot para uma caixa e duas caixas...

-- -- listas de movimentos
-- movimentos0 :: [Movimento]
-- movimentos0 = [AndarEsquerda,AndarDireita,Trepar]

-- movimentos1 :: [Movimento]
-- movimentos1 = [AndarEsquerda,AndarDireita,Trepar,InterageCaixa]

-- -- função para trabalhar com o replicateM ( para depois substituir pelo bruteForce )
-- replicateAux :: Jogo -> [[Movimento]] -> Maybe [Movimento]
-- replicateAux jogo [] = Nothing
-- replicateAux jogo (h:t)
--     | gameOver (correrMovimentos jogo h) = Just h
--     | otherwise = replicateAux jogo t

-- ----- esta funcao da numero minimo de movimentos, fazer com que para n maior acrescente movimentos inuteis tipo interage caixa
-- -- falta tambem caso de a porta estar por debaixo do jogador, usar algo tipo contEsquerda/Direita?
-- botSemCaixas :: Jogo -> [Movimento]
-- botSemCaixas (Jogo mapa (Jogador (a,b) dir val))
--     | esquerdaVazio = AndarEsquerda : botSemCaixas (moveJogador jogo AndarEsquerda)
--     | esquerdaBloco = Trepar        : botSemCaixas (moveJogador jogo Trepar)
--     | direitaVazio  = AndarDireita  : botSemCaixas (moveJogador jogo AndarDireita)
--     | direitaBloco  = Trepar        : botSemCaixas (moveJogador jogo Trepar)
--     | otherwise     = []
--     where jogo = Jogo mapa (Jogador (a,b) dir val)

--           esquerdaVazio = portaDir jogo == "oeste" && (buscaTipoMapa mapa (a-1,b) == Just Vazio || buscaTipoMapa mapa (a-1,b) == Just Porta)
--           esquerdaBloco = portaDir jogo == "oeste" &&  buscaTipoMapa mapa (a-1,b) == Just Bloco

--           direitaVazio  = portaDir jogo == "este" && (buscaTipoMapa mapa (a+1,b) == Just Vazio  || buscaTipoMapa mapa (a+1,b) == Just Porta)
--           direitaBloco  = portaDir jogo == "este" &&  buscaTipoMapa mapa (a+1,b) == Just Bloco


-- buscaPorta :: [(Peca, Coordenadas)] -> Coordenadas
-- buscaPorta [] = error "Mapa inválido, não existe porta"
-- buscaPorta ((p, (x,y)):t)
--     | p == Porta = (x,y)
--     | otherwise = buscaPorta t

-- | Função que conta o número de caixas num jogo
-- contaCaixas :: Jogo -> Int
-- contaCaixas (Jogo mapa (Jogador (a,b) dir val)) = sum (map contaCaixasAux mapa)

-- -- | Função auxiliar que conta o número de caixas numa lista de Pecas
-- contaCaixasAux :: [Peca] -> Int
-- contaCaixasAux [] = 0
-- contaCaixasAux (h:t)
--     | h == Caixa = 1 + contaCaixasAux t
--     | otherwise  = contaCaixasAux t