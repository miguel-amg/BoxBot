module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

-- Mapa nº1 do F.A.Q do projeto
mapa1Faq :: [(Peca, Coordenadas)]
mapa1Faq = 
  [ (Bloco,(0,2)),
    (Bloco,(0,3)),
    (Bloco,(0,4)),
    (Bloco,(0,5)),
    (Bloco,(0,6)),
    (Bloco,(0,7)),
    (Bloco,(0,8)),
    (Bloco,(0,9)),
    (Bloco,(0,10)),
    (Bloco,(1,0)),
    (Bloco,(1,1)),
    (Porta,(1,9)),
    (Bloco,(1,10)),
    (Bloco,(2,2)),
    (Bloco,(2,6)),
    (Bloco,(2,7)),
    (Bloco,(2,8)),
    (Bloco,(2,9)),
    (Bloco,(2,10)),
    (Bloco,(3,3)),
    (Bloco,(3,6)),
    (Bloco,(4,2)),
    (Bloco,(4,6)),
    (Bloco,(4,7)),
    (Bloco,(4,8)),
    (Bloco,(4,9)),
    (Bloco,(4,10)),
    (Bloco,(5,1)),
    (Caixa,(5,8)),
    (Bloco,(5,9)),
    (Bloco,(5,10)),
    (Bloco,(6,1)),
    (Caixa,(6,8)),
    (Bloco,(6,9)),
    (Bloco,(7,1)),
    (Bloco,(7,9)),
    (Bloco,(8,1)),
    (Bloco,(8,8)),
    (Bloco,(8,9)),
    (Bloco,(9,1)),
    (Bloco,(9,7)),
    (Bloco,(9,8)),
    (Bloco,(9,9)),
    (Bloco,(9,10)),
    (Bloco,(10,1)),
    (Bloco,(10,10)),
    (Bloco,(11,1)),
    (Bloco,(11,9)),
    (Bloco,(11,10)),
    (Bloco,(12,1)),
    (Bloco,(12,7)),
    (Bloco,(12,8)),
    (Bloco,(12,9)),
    (Bloco,(13,1)),
    (Bloco,(13,6)),
    (Bloco,(13,7)),
    (Bloco,(14,1)),
    (Caixa,(14,6)),
    (Bloco,(14,7)),
    (Bloco,(15,1)),
    (Bloco,(15,7)),
    (Bloco,(16,1)),
    (Caixa,(16,5)),
    (Bloco,(16,6)),
    (Bloco,(16,7)),
    (Bloco,(17,1)),
    (Caixa,(17,4)),
    (Caixa,(17,5)),
    (Bloco,(17,6)),
    (Bloco,(18,2)),
    (Bloco,(18,3)),
    (Bloco,(18,4)),
    (Bloco,(18,5))
  ]

-- Identico ao mapa anterior, mas sem um bloco no chão
mapa1FaqDescontinuo :: [(Peca, Coordenadas)]
mapa1FaqDescontinuo = 
  [ (Bloco,(0,2)),
    (Bloco,(0,3)),
    (Bloco,(0,4)),
    (Bloco,(0,5)),
    (Bloco,(0,6)),
    (Bloco,(0,7)),
    (Bloco,(0,8)),
    (Bloco,(0,9)),
    (Bloco,(0,10)),
    (Bloco,(1,0)),
    (Bloco,(1,1)),
    (Porta,(1,9)),
    (Bloco,(1,10)),
    (Bloco,(2,2)),
    (Bloco,(2,6)),
    (Bloco,(2,7)),
    (Bloco,(2,8)),
    (Bloco,(2,9)),
    (Bloco,(2,10)),
    (Bloco,(3,3)),
    (Bloco,(3,6)),
    (Bloco,(4,2)),
    (Bloco,(4,6)),
    (Bloco,(4,7)),
    (Bloco,(4,8)),
    (Bloco,(4,9)),
    (Bloco,(4,10)),
    (Bloco,(5,1)),
    (Bloco,(5,6)),
    (Caixa,(5,8)),
    (Bloco,(6,1)),
    (Caixa,(6,8)),
    (Bloco,(6,9)),
    (Bloco,(7,1)),
    (Bloco,(7,9)),
    (Bloco,(8,1)),
    (Bloco,(8,8)),
    (Bloco,(8,9)),
    (Bloco,(9,1)),
    (Bloco,(9,7)),
    (Bloco,(9,8)),
    (Bloco,(9,9)),
    (Bloco,(9,10)),
    (Bloco,(10,1)),
    (Bloco,(10,10)),
    (Bloco,(11,1)),
    (Bloco,(11,9)),
    (Bloco,(11,10)),
    (Bloco,(12,1)),
    (Bloco,(12,7)),
    (Bloco,(12,8)),
    (Bloco,(12,9)),
    (Bloco,(13,1)),
    (Bloco,(13,6)),
    (Bloco,(13,7)),
    (Bloco,(14,1)),
    (Caixa,(14,6)),
    (Bloco,(14,7)),
    (Bloco,(15,1)),
    (Bloco,(15,7)),
    (Bloco,(16,1)),
    (Caixa,(16,5)),
    (Bloco,(16,6)),
    (Bloco,(16,7)),
    (Bloco,(17,1)),
    (Caixa,(17,4)),
    (Caixa,(17,5)),
    (Bloco,(17,6)),
    (Bloco,(18,2)),
    (Bloco,(18,3)),
    (Bloco,(18,4)),
    (Bloco,(18,5))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r :: Mapa --mapa parecido com m1r
m2r =  
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m3r :: Mapa --mapa parecido com m1r
m3r =  
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Porta, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco, Caixa],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m4r :: Mapa --mapa em triangulo para trepar
m4r =  
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Caixa, Vazio],
    [Porta, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Caixa, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m4r2 :: Mapa --mapa igual ao anterior mas sem caixa
m4r2 =  
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Caixa, Vazio],
    [Porta, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Caixa, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m4r3 :: Mapa --mapa em triangulo para trepar
m4r3 =  
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Caixa, Vazio],
    [Porta, Vazio, Bloco, Bloco, Bloco, Bloco, Caixa, Caixa, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m4r4 :: Mapa --mapa parecido com m4r
m4r4 =  
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Caixa, Vazio],
    [Porta, Caixa, Bloco, Bloco, Bloco, Bloco, Vazio, Caixa, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

mapatecto :: Mapa -- mapa inválido, é só um tecto
mapatecto =
  [ [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  ]


mapaContinuo :: Mapa -- chao válido
mapaContinuo =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  ]

mapaBlocosSoltos :: Mapa -- chao inválido
mapaBlocosSoltos =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio]
  ]

mapaZigZag :: Mapa -- chao válido
mapaZigZag =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  ]  

mapaZigZag2 :: Mapa -- chao inválido, com bloco solto
mapaZigZag2 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio]
  ]
m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

-- Mapa sem portas
mapaSP :: [(Peca, Coordenadas)]
mapaSP =
  [ (Bloco, (0, 4)),   
    (Bloco, (1, 4)),  
    (Bloco, (2, 4)),  
    (Bloco, (3, 4)),  
    (Bloco, (4, 4)),  
    (Bloco, (5, 4)),  
    (Bloco, (6, 4)),
    (Caixa, (3, 3))
  ]

-- Mapa com uma caixa flutuante
mapaCF :: [(Peca, Coordenadas)]
mapaCF =
  [ (Porta, (0, 3)),  
    (Bloco, (0, 4)),  -- Chão   
    (Bloco, (1, 4)),  -- Chão  
    (Bloco, (2, 4)),  -- Chão  
    (Bloco, (3, 4)),  -- Chão  
    (Bloco, (4, 4)),  -- Chão  
    (Bloco, (5, 4)),  -- Chão  
    (Bloco, (6, 4)),  -- Chão
    (Caixa, (2, 2))   -- Caixa Flutuante
  ]

-- Mapa com várias caixas empilhadas
mapaCF2 :: [(Peca, Coordenadas)]
mapaCF2 =
  [ (Porta, (0, 3)),  
    (Bloco, (0, 4)),  -- Chão   
    (Bloco, (1, 4)),  -- Chão  
    (Bloco, (2, 4)),  -- Chão  
    (Bloco, (3, 4)),  -- Chão  
    (Bloco, (4, 4)),  -- Chão  
    (Bloco, (5, 4)),  -- Chão  
    (Bloco, (6, 4)),  -- Chão
    (Caixa, (6, 3)),  -- Caixa Empilhada  
    (Caixa, (6, 2)),  -- Caixa Empilhada
    (Caixa, (6, 1)),  -- Caixa Empilhada
    (Caixa, (6, 0))   -- Caixa Empilhada
  ]

mapaEscada :: Mapa
mapaEscada =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio],
    [Porta, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

mapaEscadaShow :: Jogo
mapaEscadaShow = Jogo mapaEscada (Jogador (6, 3) Oeste False)

------------------------------------------------------------------ Testes Tarefa 6

jogoD1 :: Jogo -- resolveJogo 4 jogoD1 == Just [AndarDireita,AndarDireita,AndarDireita,AndarDireita]
jogoD1 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Porta]
              ,[Bloco,Bloco,Bloco,Bloco,Bloco]]
        (Jogador (0,2) Este False)

jogoD2 :: Jogo -- resolveJogo 9 jogoD2 == Just [AndarDireita,AndarDireita,Trepar,AndarDireita,AndarDireita,Trepar,AndarDireita,AndarDireita,AndarDireita]
jogoD2 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Porta]
              ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
         (Jogador (0,2) Este False)

jogoE1 :: Jogo -- resolveJogo 13 jogoE1 == Just [AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda]
jogoE1 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Porta,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio]
              ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
         (Jogador (13,2) Este False)


jogoE2 :: Jogo -- resolveJogo 100 jogoE2 == Nothing
jogoE2 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Porta,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio]
              ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
         (Jogador (13,4) Este False)

jogoC1 :: Jogo -- resolveJogo 14 jogoC1 == Just [AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda]
jogoC1 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Porta,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio]
              ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
         (Jogador (8,3) Este False)

jogoC2 :: Jogo -- resolveJogo 29 jogoC2 == Just [AndarEsquerda,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,InterageCaixa,AndarEsquerda,Trepar,AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
jogoC2 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio]
              ,[Porta,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio]
              ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
         (Jogador (20,3) Este False)

jogo6C :: Jogo -- 39 \ 41   resolveJogo 41 jogoC2 ==
jogo6C = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
              ,[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio]
              ,[Porta,Bloco,Caixa,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Bloco,Vazio,Caixa,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Caixa]
              ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
         (Jogador (25,3) Este False)

jogobv2c :: Jogo  -- resolveJogo 24 jogobv2c == Just [AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita]
jogobv2c = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
                ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
                ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio]
                ,[Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Porta]
                ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco]
                ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio]]
           (Jogador (2,3) Este False)

jogobbb :: Jogo -- resolveJogo 26 jogobbb == Just [AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,Trepar,AndarDireita]
jogobbb = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
               ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
               ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]
               ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]
               ,[Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Porta]
               ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
          (Jogador (4,4) Este False)

jogobbb2 :: Jogo  -- resolveJogo 95 jogobbb2 == Just [AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,Trepar,AndarDireita]
jogobbb2 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
                ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
                ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]
                ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]
                ,[Caixa,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Porta]
                ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
                ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
           (Jogador (5,4) Este False)

-- jogo com caixas empilhadas
jogo5 :: Jogo
jogo5 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
             ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
             ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]
             ,[Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]
             ,[Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Porta]
             ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
        (Jogador (3,4) Este False)
