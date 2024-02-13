{- |
Module      : Tarefa5_2021li1g074
Description : Aplicação gráfica completa
Copyright   : Miguel Ângelo Martins Guimarães;
            : Filipe Prudêncio Pacheco dos Santos;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where

-- Importar gloss
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import System.Exit

-- Import dos modulos essenciais 
import LI12122
import FuncoesAuxiliares
import Tarefa1_2021li1g074
import Tarefa2_2021li1g074
import Tarefa3_2021li1g074
import Tarefa4_2021li1g074

{- Ajuda
Comandos do menu - Alternar opção: setas
                   Selecionar opção : enter

Comandos do editor- Mexer cursor : setas
                    Mexer camera : wasd 
                    Alterar peça: 'q' e 'e'
                    Colocar peça : Barra espaço
                    Alternar opção do menu: Tab
                    Selecionar opção do menu: Enter
                    centrar cursor : h 
                    centrar camera : g 
-}

 ------------------------------------------  Configurações do jogo ------------------------------------------ 

{-| Determina a janela do jogo -}
janelaModo :: Display
janelaModo = FullScreen -- Jogar em tela cheia

{-|  Determina a cor do fundo do jogo -}
janelaFundo :: Color
janelaFundo = makeColorI 47 0 40 0  -- Criar cor RGB de tonalidade vermelha

{-|  Numero de quadros por segundo -}
framesPS :: Int
framesPS = 60

 -------------------------------------------------- Main ----------------------------------------------------

-- | Função principal
main :: IO ()
main =
    do  -- Imagens essenciais do mapa (Imagens das peças do jogo e do jogador) 
        caixaBMP         <- loadBMP "recursos/essencial/caixa.bmp"        -- Imagem da caixa
        blocoBMP         <- loadBMP "recursos/essencial/bloco.bmp"        -- Imagem do bloco
        portaBMP         <- loadBMP "recursos/essencial/porta.bmp"        -- Imagem da porta
        jogadorOeste     <- loadBMP "recursos/essencial/jogadorOeste.bmp" -- Imagem do jogador, direção Oeste
        jogadorEste      <- loadBMP "recursos/essencial/jogadorEste.bmp"  -- Imagem do jogador, direção este
        carregaOeste     <- loadBMP "recursos/essencial/carregaOeste.bmp" -- Imagem do jogador a carregar uma caixa, direção Oeste
        carregaEste      <- loadBMP "recursos/essencial/carregaEste.bmp"  -- Imagem do jogador a carregar uma caixa, direção Este

        -- Imagens do Menu principal
        menu1play        <- loadBMP "recursos/menu/menu1/m1play.bmp"      -- Opção (jogar) do menu principal
        menu1levels      <- loadBMP "recursos/menu/menu1/m1nivel.bmp"     -- Opção (niveis) do menu principal
        menu1editor      <- loadBMP "recursos/menu/menu1/m1editor.bmp"    -- Opção (editor) do menu principal
        menu1exit        <- loadBMP "recursos/menu/menu1/m1exit.bmp"      -- Opção (sair) do menu principal

        -- Imagens do Menu 2
        menu2continue    <- loadBMP "recursos/menu/menu2/m2cont.bmp"      -- Opção (continuar) do menu 2
        menu2restart     <- loadBMP "recursos/menu/menu2/m2rec.bmp"       -- Opção (recomecar) do menu 2
        menu2levels      <- loadBMP "recursos/menu/menu2/m2nivel.bmp"     -- Opção (niveis) do menu 2
        menu2editor      <- loadBMP "recursos/menu/menu2/m2editor.bmp"    -- Opção (editor) do menu 2
        menu2exit        <- loadBMP "recursos/menu/menu2/m2exit.bmp"      -- Opção (sair) do menu 2

        -- Imagens do Menu de Pausa (Menu de jogo)
        menuJogoCont     <- loadBMP "recursos/menu/menuJogo/mJCont.bmp"   -- Opção (continuar) do menu de pausa
        menuJogoRec      <- loadBMP "recursos/menu/menuJogo/mJRec.bmp"    -- Opção (recomecar) do menu de pausa
        menuJogoVolt     <- loadBMP "recursos/menu/menuJogo/mJVoltar.bmp" -- Opção (menu inicial) do menu de pausa
        menuJogoRes      <- loadBMP "recursos/menu/menuJogo/mJRes.bmp"    -- Opção (resolver) do menu de pausa (NAO UTILIZADO)
        menuJogoSair     <- loadBMP "recursos/menu/menuJogo/mJSair.bmp"   -- Opção (continuar) do menu de pausa

        -- Imagens selecionador de niveis e imagem do fim de jogo
        nivel1           <- loadBMP "recursos/menu/selLvl1.bmp"           -- Selecionar nivel 1 (Campanha)
        nivel2           <- loadBMP "recursos/menu/selLvl2.bmp"           -- Selecionar nivel 2 (Campanha)
        nivel3           <- loadBMP "recursos/menu/selLvl3.bmp"           -- Selecionar nivel 3 (Campanha)
        selecioneVoltar  <- loadBMP "recursos/menu/selVoltar.bmp"         -- Opção voltar 
        theEnd           <- loadBMP "recursos/menu/theEnd.bmp"            -- Tela de fim de jogo 

        -- Imagens do modo Editor
        cursor           <- loadBMP "recursos/editor/cursor.bmp"          -- (Editor) Imagem do cursor
        selBloco         <- loadBMP "recursos/editor/selectBloco.bmp"     -- (Editor) Menu de peças - bloco selecionado
        selCaixa         <- loadBMP "recursos/editor/selectCaixa.bmp"     -- (Editor) Menu de peças - caixa selecionada
        selPorta         <- loadBMP "recursos/editor/selectPorta.bmp"     -- (Editor) Menu de peças - porta selecionada
        selJogador       <- loadBMP "recursos/editor/selectJogador.bmp"   -- (Editor) Menu de peças - jogador selecionado
        selDelete        <- loadBMP "recursos/editor/selectDelete.bmp"    -- (Editor) Menu de peças - lixo selecionado
        cursorDel        <- loadBMP "recursos/editor/cursorDel.bmp"       -- (Editor) Imagem do cursor delete
        selValidar       <- loadBMP "recursos/editor/selValidar.bmp"      -- (Editor) Menu de opções - opção validar 
        selTestar        <- loadBMP "recursos/editor/selTestar.bmp"       -- (Editor) Menu de opções - opção testar
        selSair          <- loadBMP "recursos/editor/selSair.bmp"         -- (Editor) Menu de opções - opção sair
        cursorB          <- loadBMP "recursos/editor/cursorB.bmp"         -- (Editor) Imagem do cursor (bloco selecionado)
        cursorC          <- loadBMP "recursos/editor/cursorC.bmp"         -- (Editor) Imagem do cursor (caixa selecionada)
        cursorJ          <- loadBMP "recursos/editor/cursorJ.bmp"         -- (Editor) Imagem do cursor (jogador selecionado)
        cursorP          <- loadBMP "recursos/editor/cursorP.bmp"         -- (Editor) Imagem do cursor (porta selecionada)
        ref              <- loadBMP "recursos/editor/referencia.bmp"      -- (Editor) Referencia do origin 
        borda            <- loadBMP "recursos/editor/borda.bmp"           -- (Editor) Borda
        selAbrir         <- loadBMP "recursos/editor/selAbrir.bmp"        -- (Editor) Menu de opções - opção carregar 
        selGravar        <- loadBMP "recursos/editor/selGravar.bmp"       -- (Editor) Menu de opções - opção gravar

        -- Imagens save e load game
        loadGame         <- loadBMP "recursos/menu/menu1/m1load.bmp"
        saveGame         <- loadBMP "recursos/menu/menuJogo/m1save.bmp"
        saveScreen       <- loadBMP "recursos/menu/menuJogo/mjGsaved.bmp"

        -- Imagens da tela de gravação e de carregamento de mapas do editor
        load1e           <- loadBMP "recursos/editor/load/load1e.bmp"
        load2e           <- loadBMP "recursos/editor/load/load2e.bmp"
        load3e           <- loadBMP "recursos/editor/load/load3e.bmp"
        save1e           <- loadBMP "recursos/editor/save/save1e.bmp"
        save2e           <- loadBMP "recursos/editor/save/save2e.bmp"
        save3e           <- loadBMP "recursos/editor/save/save3e.bmp"

        -- Restantes imagens do selecionador de niveis
        nivel4            <- loadBMP "recursos/menu/selLvl4.bmp"
        nivel5            <- loadBMP "recursos/menu/selLvl5.bmp"
        nivel1e           <- loadBMP "recursos/menu/selLvl1e.bmp"
        nivel2e           <- loadBMP "recursos/menu/selLvl2e.bmp"
        nivel3e           <- loadBMP "recursos/menu/selLvl3e.bmp"
        nivel4e           <- loadBMP "recursos/menu/selLvl4e.bmp"
        nivel5e           <- loadBMP "recursos/menu/selLvl5e.bmp"

        -- Imagens dos avisos e intros do editor 
        avisoL1           <- loadBMP "recursos/avisos/avisoLoad1.bmp"
        avisoL2           <- loadBMP "recursos/avisos/avisoLoad2.bmp"
        avisoT            <- loadBMP "recursos/avisos/avisoTeste.bmp"
        avisoP            <- loadBMP "recursos/avisos/avisoPortas.bmp"
        avisoSP           <- loadBMP "recursos/avisos/avisoSPortas.bmp"
        avisoCa           <- loadBMP "recursos/avisos/avisoCaixas.bmp"
        avisoCh           <- loadBMP "recursos/avisos/avisoChao.bmp"
        avisoSuc          <- loadBMP "recursos/avisos/avisoSucesso.bmp"
        introEditor       <- loadBMP "recursos/intros/introEditor.bmp"

        -- Imagens das intros dos niveis
        intro1            <- loadBMP "recursos/intros/intro1.bmp"
        intro2            <- loadBMP "recursos/intros/intro2.bmp"
        intro3            <- loadBMP "recursos/intros/intro3.bmp"
        intro4            <- loadBMP "recursos/intros/intro4.bmp"
        intro5            <- loadBMP "recursos/intros/intro5.bmp"
        intro1e           <- loadBMP "recursos/intros/intro1e.bmp"
        intro2e           <- loadBMP "recursos/intros/intro2e.bmp"
        intro3e           <- loadBMP "recursos/intros/intro3e.bmp"
        intro4e           <- loadBMP "recursos/intros/intro4e.bmp"
        intro5e           <- loadBMP "recursos/intros/intro5e.bmp"
        avisoGravar       <- loadBMP "recursos/avisos/avisoGravar.bmp" -- Aviso de gravação do editor

        -- Imagens adicionadas tarde ->
        -- Restantes imagens da tela de carregamento e gravação do editor
        load4e           <- loadBMP "recursos/editor/load/load4e.bmp" -- Carregar nivel 4
        load5e           <- loadBMP "recursos/editor/load/load5e.bmp" -- Carregar nivel 5
        save4e           <- loadBMP "recursos/editor/save/save4e.bmp" -- Gravar nivel 4
        save5e           <- loadBMP "recursos/editor/save/save5e.bmp" -- Gravar nivel 5

        m1info           <- loadBMP "recursos/menu/menu1/m1info.bmp"  -- Opcao info do menu inicial
        infoTela         <- loadBMP "recursos/menu/info.bmp"          -- Imagem da tela de info
        menu2load        <- loadBMP "recursos/menu/menu2/m2load.bmp"  -- Opção (Load) do menu 2

        let pics = [caixaBMP, blocoBMP, portaBMP, jogadorOeste, jogadorEste, carregaOeste, carregaEste                             -- Imagens essenciais
                   ,menu1play, menu1levels, menu1editor, menu1exit                                                                 -- Menu principal
                   ,menu2continue, menu2restart, menu2levels,menu2editor, menu2exit                                                -- Menu 2
                   ,menuJogoCont, menuJogoRec, menuJogoVolt, menuJogoRes, menuJogoSair                                             -- Menu de jogo/pausa
                   ,nivel1, nivel2, nivel3, selecioneVoltar                                                                        -- Selecionar niveis
                   ,theEnd                                                                                                         -- Imagem do fim de jogo
                   ,cursor, selBloco, selCaixa, selPorta, selJogador, selDelete, cursorDel, selValidar, selTestar, selSair         -- Imagems do editor
                   ,cursorB, cursorC, cursorJ, cursorP, ref, borda, selAbrir, selGravar                                            -- Imagens do editor
                   ,loadGame, saveGame, saveScreen, load1e, load2e, load3e, save1e, save2e, save3e                                 -- Imagens misc
                   ,nivel4 ,nivel5 ,nivel1e ,nivel2e ,nivel3e ,nivel4e ,nivel5e                                                    -- Selecionar niveis
                   ,avisoL1 ,avisoL2 ,avisoT ,avisoP, avisoSP, avisoCa, avisoCh, avisoSuc                                          -- Imagens dos avisos do editor
                   ,introEditor ,intro1 ,intro2 ,intro3 ,intro4 ,intro5 ,intro1e ,intro2e ,intro3e ,intro4e ,intro5e ,avisoGravar  -- Imagens das intros
                   ,load4e ,load5e ,save4e ,save5e ,m1info ,infoTela ,menu2load]                                                   -- Avisos e intros

        playIO
            janelaModo      -- O modo da janela (Fullscreen)
            janelaFundo     -- A cor de fundo do jogo
            framesPS        -- O número de quadros por segundo  
            estadoInicial   -- (MenuInicial Jogar, jogoTeste)          
            (drawIO pics)   -- Transforma os (Menus/Jogos) em Picture
            eventIO         -- Eventos do jogo
            (const return)  -- Reagir á passagem do tempo (Desativado)


-----------------------------------------------------------------------------------------------------
------------------------------------------      Mundo      ------------------------------------------ 
-----------------------------------------------------------------------------------------------------


{-| Tipo Mundo que contém tudo que é necessario para o jogo -}
type World = (Menu, Jogo, Editor)

{-| Tipo Menu que define os menus existentes no jogo e as suas opções/parametros -}
data Menu = MenuInicial Opcoes Int | MenuInicial2 Opcoes Int | MenuNiveis Opcoes Int | MenuJogo Opcoes Int
          | ModoJogo Int  | ModoJogoTeste | VenceuJogo
          | InEditor | Save Int | InEditorOps Modo Int Coordenadas| Aviso Int Int | Intro Int | InfoTela
    deriving (Show, Read, Eq, Ord)

{-| Tipo Opções que define as opções possiveis para os menus do jogo -}
data Opcoes = Jogar | SelecionarNivel | Sair | LoadGame {-Menu 2 :-} | Continuar | Restart                           -- Menu Principal
            | ContinuarJogo | RestartJogo | SaveGame | ReturnMainMenu | SolveLevel                                   -- Menu Jogo
            | Level1 | Level2 | Level3 | Level4 | Level5 | Level1e | Level2e | Level3e | Level4e | Level5e | Voltar  -- Menu de seleção de Niveis
            | EditorOp | Info
    deriving (Show, Read, Eq, Ord)

{-| Tipo Editor que define os parametros do mesmo (coordenadas do cursor, peça selecionada, mapa , coordenadas do player, opcao selecionada , coodenadas da camera, mapa validado, camera Lock) -}
data Editor = Editor Coordenadas Int [(Peca,Coordenadas)] Coordenadas Int Coordenadas Bool Bool
    deriving (Show, Read, Eq, Ord)

{-| Tipo Modo que define o modo (tela de carregamento/gravação) em que nos encontramos -}
data Modo = Gravar | Carregar
    deriving (Show, Read, Eq, Ord)

-----------------------------------------------------------------------
--                           Estado Inicial                          --
-----------------------------------------------------------------------

{-| Define o estado inicial do jogo -}
estadoInicial :: World
estadoInicial = (MenuInicial Jogar 1, jogo1, editorInicio)

{-| Define o estado inicial do editor -}
editorInicio :: Editor
editorInicio = Editor (0,0)   1  mapaStart (-2, 2)  4  (0,0)  False  False
--                    cursor sel   mapa    player sel2 camera valido CLock

{-| Define o mapa predefinido do editor -}
mapaStart :: [(Peca,Coordenadas)]
mapaStart = [(Bloco,(-2,3)),(Bloco,(-1,3)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Porta,(2,2))]


-----------------------------------------------------------------------------------------------------
------------------------------------------  Desenhar Jogo  ------------------------------------------ 
-----------------------------------------------------------------------------------------------------

{-| Função que recebe uma lista de imagens e associa a cada uma um determinado número -}
selecionaPic :: [Picture] -> Int -> Picture
selecionaPic [] _ = Blank
selecionaPic [x0,x1,x2,x3,x4,x5,x6                           -- Imagens essenciais
             ,x7,x8,x9,x10                                   -- Imagens do menu principal
             ,x11,x12,x13,x14,x15                            -- Imagens do menu 2
             ,x16,x17,x18,x19,x20                            -- Imagens do menu de jogo/pausa
             ,x21,x22,x23,x24                                -- Seleção de niveis
             ,x25                                            -- Imagem de fim de jogo
             ,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35        -- Imagens do editor
             ,x36,x37,x38,x39,x40,x41,x42,x43                -- Imagens do editor
             ,x44,x45,x46,x47,x48,x49,x50,x51,x52            -- Imagens do save e load 
             ,x53,x54,x55,x56,x57,x58,x59                    -- Imagens do selecionador de niveis
             ,x60,x61,x62,x63,x64,x65,x66,x67
             ,x68,x69,x70,x71,x72,x73,x74,x75,x76,x77,x78,x79
             ,x80,x81,x82,x83,x84                            -- Avisos e intros
             ,x85,x86]                                       -- Info tela ,menu2load
  val =
    case val of 0 -> x0 -- Caixa
                1 -> x1 -- Bloco
                2 -> x2 -- Porta
                3 -> x3 -- Jogador Oeste
                4 -> x4 -- Jogador Este
                5 -> x5 -- Carregar Caixa Oeste
                6 -> x6 -- Carregar Caixa Este

                7  -> x7  -- (Menu principal) play   
                8  -> x8  -- (Menu principal) levels 
                9  -> x9  -- (Menu principal) editor 
                10 -> x10 -- (Menu principal) exit   

                11 -> x11 -- (Menu 2) continuar
                12 -> x12 -- (Menu 2) recomeçar
                13 -> x13 -- (Menu 2) levels 
                14 -> x14 -- (Menu 2) editor 
                15 -> x15 -- (Menu 2) exit   

                16 -> x16 -- (Menu de pausa) continuar
                17 -> x17 -- (Menu de pausa) restart
                18 -> x18 -- (Menu de pausa) return
                19 -> x19 -- (Menu de pausa) solve
                20 -> x20 -- (Menu de pausa) exit

                21 -> x21 -- (Menu de seleção de niveis) nivel 1
                22 -> x22 -- (Menu de seleção de niveis) nivel 2
                23 -> x23 -- (Menu de seleção de niveis) nivel 3
                24 -> x24 -- (Menu de seleção de niveis) voltar

                25 -> x25 -- Imagem do fim de jogo

                26 -> x26 -- (Editor) cursor
                27 -> x27 -- (Editor) selbloco
                28 -> x28 -- (Editor) selcaixa
                29 -> x29 -- (Editor) selporta
                30 -> x30 -- (Editor) seljogador
                31 -> x31 -- (Editor) seldelete
                32 -> x32 -- (Editor) cursorDel
                33 -> x33 -- (Editor) selValidar
                34 -> x34 -- (Editor) selTestar
                35 -> x35 -- (Editor) selSair
                36 -> x36 -- (Editor) cursorB
                37 -> x37 -- (Editor) cursorC
                38 -> x38 -- (Editor) cursorJ
                39 -> x39 -- (Editor) cursorP
                40 -> x40 -- (Editor) referencia
                41 -> x41 -- (Editor) borda
                42 -> x42 -- (Editor) selAbrir
                43 -> x43 -- (Editor) selGravar

                44 -> x44 -- load game
                45 -> x45 -- save game
                46 -> x46 -- save screen

                47 -> x47 -- load1e
                48 -> x48 -- load2e
                49 -> x49 -- load3e

                50 -> x50 -- save1e
                51 -> x51 -- save2e
                52 -> x52 -- save3e

                53 -> x53 -- (Menu de seleção de niveis) Nivel 4 campanha
                54 -> x54 -- (Menu de seleção de niveis) Nivel 5 campanha
                55 -> x55 -- (Menu de seleção de niveis) Nivel 1 editor
                56 -> x56 -- (Menu de seleção de niveis) Nivel 2 editor
                57 -> x57 -- (Menu de seleção de niveis) Nivel 3 editor
                58 -> x58 -- (Menu de seleção de niveis) Nivel 4 editor
                59 -> x59 -- (Menu de seleção de niveis) Nivel 5 editor

                60 -> x60 -- Aviso Load 1 
                61 -> x61 -- Aviso Load 2 
                62 -> x62 -- Aviso Teste
                63 -> x63 -- Aviso porta
                64 -> x64 -- Aviso Sem Porta
                65 -> x65 -- Aviso Caixas
                66 -> x66 -- Aviso Chao
                67 -> x67 -- Aviso Sucesso

                68 -> x68 -- Intro editor
                69 -> x69 -- Intro 1
                70 -> x70 -- Intro 2
                71 -> x71 -- Intro 3
                72 -> x72 -- Intro 4
                73 -> x73 -- Intro 5
                74 -> x74 -- Intro 1e
                75 -> x75 -- Intro 2e
                76 -> x76 -- Intro 3e
                77 -> x77 -- Intro 4e
                78 -> x78 -- Intro 5e   
                79 -> x79 -- Aviso Gravar   

                80 -> x80 -- load4e
                81 -> x81 -- load5e
                82 -> x82 -- save4e
                83 -> x83 -- save5e

                84 -> x84 -- m1info
                85 -> x85 -- infoTela
                86 -> x86 -- Menu 2 load


                _ -> Blank
selecionaPic _ _   = error "picture doesn't exist"
-- Atualizar acima


------------------------------------------------------------------------------------------------
------------------------------------------  Desenhar  ------------------------------------------ 
------------------------------------------------------------------------------------------------

-- | Função que tranforma as Pictures dadas pela função 'draw' em IO Pictures
drawIO :: [Picture] -> World -> IO Picture
drawIO pics world = return (draw pics world)

-----------------------------------------------------------------------
--                           Desenhar Menus                          --
-----------------------------------------------------------------------

-- | Função que desenha o estado do jogo
draw :: [Picture] -> World -> Picture

-- Desenhar o menu principal e as suas opções
draw pics (MenuInicial Jogar _, jogo, e) = selecionaPic pics 7
draw pics (MenuInicial LoadGame _, jogo, e) = selecionaPic pics 44
draw pics (MenuInicial SelecionarNivel _, jogo, e) = selecionaPic pics 8
draw pics (MenuInicial EditorOp _, jogo, e) = selecionaPic pics 9
draw pics (MenuInicial Info _, jogo, e) = selecionaPic pics 84
draw pics (MenuInicial Sair _, jogo, e)  = selecionaPic pics 10

-- Desenhar o Menu 2
draw pics (MenuInicial2 Continuar _, jogo, e) = selecionaPic pics 11
draw pics (MenuInicial2 Restart _, jogo, e) = selecionaPic pics 12
draw pics (MenuInicial2 LoadGame _, jogo, e) = selecionaPic pics 86
draw pics (MenuInicial2 SelecionarNivel _, jogo, e) = selecionaPic pics 13
draw pics (MenuInicial2 EditorOp _, jogo, e) = selecionaPic pics 14
draw pics (MenuInicial2 Sair _, jogo, e)  = selecionaPic pics 15

-- Menu do modo de jogo (Desenha o modo de jogo juntamente com a devida imagem da opcao, o modo de jogo e desenhado devido á transparencia da imagem)
draw pics (MenuJogo ContinuarJogo _ , Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs)),boxBotMarca]),selecionaPic pics 16]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)
draw pics (MenuJogo RestartJogo _, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs)),boxBotMarca]),selecionaPic pics 17]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)
draw pics (MenuJogo SaveGame _, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs)),boxBotMarca]),selecionaPic pics 45]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)
draw pics (MenuJogo ReturnMainMenu _, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs)),boxBotMarca]),selecionaPic pics 18]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)
draw pics (MenuJogo SolveLevel _, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs)),boxBotMarca]),selecionaPic pics 19]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)
draw pics (MenuJogo Sair _, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs)),boxBotMarca]),selecionaPic pics 20]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)

-- Desenhar a tela de gravação
draw pics (Save _, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs)),boxBotMarca]),selecionaPic pics 46]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)

-- Desenhar o selecionador de niveis
draw pics (MenuNiveis Level1 _, jogo, e) = selecionaPic pics 21  -- Desenha o nivel 1 (Campanha)
draw pics (MenuNiveis Level2 _, jogo, e) = selecionaPic pics 22  -- Desenha o nivel 2 (Campanha)
draw pics (MenuNiveis Level3 _, jogo, e) = selecionaPic pics 23  -- Desenha o nivel 3 (Campanha)
draw pics (MenuNiveis Level4 _, jogo, e) = selecionaPic pics 53  -- Desenha o nivel 4 (Campanha)
draw pics (MenuNiveis Level5 _, jogo, e) = selecionaPic pics 54  -- Desenha o nivel 5 (Campanha)
draw pics (MenuNiveis Level1e _, jogo, e) = selecionaPic pics 55 -- Desenha o nivel 1 (Editor)
draw pics (MenuNiveis Level2e _, jogo, e) = selecionaPic pics 56 -- Desenha o nivel 2 (Editor)
draw pics (MenuNiveis Level3e _, jogo, e) = selecionaPic pics 57 -- Desenha o nivel 3 (Editor)
draw pics (MenuNiveis Level4e _, jogo, e) = selecionaPic pics 58 -- Desenha o nivel 4 (Editor)
draw pics (MenuNiveis Level5e _, jogo, e) = selecionaPic pics 59 -- Desenha o nivel 5 (Editor)
draw pics (MenuNiveis Voltar _, jogo, e) = selecionaPic pics 24  -- Desenha a opção voltar

-- Fim de jogo
draw pics (VenceuJogo, jogo, e) = selecionaPic pics 25

-----------------------------------------------------------------------
--                        Desenhar modo de Jogo                      --
-----------------------------------------------------------------------

-- Desenhar o modo de jogo 
draw pics (ModoJogo nivel, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs))]),Pictures [boxBotMarca,nivelMarca nivel]]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)

-- Desenhar o modo de jogo teste 
draw pics (ModoJogoTeste, Jogo (x:xs) (Jogador (a,b) dir val), e) = Pictures [Translate (fromIntegral (c1*64)) (fromIntegral (c2*64)) (Pictures [ajustarMapa (Pictures (desenhaJogador pics (Jogador (a,b) dir val) : desenhaMapa pics (desconstroiMapa (x:xs)))) (maxX x,maxY (x:xs))]),boxBotMarca]
    where (c1,c2) = cameraNecessaria (a,b) (x:xs)

-----------------------------------------------------------------------
--                          Desenhar Editor                          --
-----------------------------------------------------------------------

draw pics (InEditor, jogo , Editor (a,b) sel mapa player sel2 (c,d) val cl) =
    Pictures (Translate (fromIntegral (c*64)) (fromIntegral (d*64)) (Pictures ( ref : jogadorPic : mapaPic : [cursorPic])) : cLock : indVal : menu1 : menu2 : borda : [Blank] )
    where cLock = cameraLock cl                                         -- Desenha o indicador de camera lock
          menu1 = desenhaSelection pics sel                             -- Desenha o menu de peças
          menu2 = desenhaSelection2 pics sel2                           -- Desenha o menu de opções
          cursorPic = desenhaCursorX sel (a,b) pics                     -- Desenha os diferentes cursores do editor
          ref =  Scale 2.0 2.0 (selecionaPic pics 40)                   -- Desenha o origin
          jogadorPic = desenhaJogador pics (Jogador player Este False)  -- Desenha o jogador
          mapaPic = desenhaMapaSingle pics mapa                         -- Desenha o mapa
          indVal =  indValidado val                                     -- Desenha a indicação da validação do mapa
          borda = selecionaPic pics 41                                  -- Desenha a borda do editor

-----------------------------------------------------------------------
--                 Desenhar Editor Opções (InEditorOps)              --
-----------------------------------------------------------------------

draw pics (InEditorOps modo sel (c1,c2), jg , Editor pos s m (x,y) s2 c val cl)  = Pictures [Translate (fromIntegral (-c1*19)) (fromIntegral (c2*19))(Scale 0.3 0.3 jogoDraw),opDraw pics sel modo,indicaDimensao m (x,y) ]
    where jogoDraw = Pictures (desenhaJogador pics (Jogador (x,y) Este False) : desenhaMapa pics m)

-----------------------------------------------------------------------
--                        Desenhar Avisos                            --
-----------------------------------------------------------------------

-- Desenha os avisos
draw pics (Aviso ind1 ind2, jogo , Editor (a,b) sel mapa player sel2 (c,d) val cl) =
    Pictures [Pictures (Translate (fromIntegral (c*64)) (fromIntegral (d*64)) (Pictures ( ref : jogadorPic : mapaPic : [cursorPic])) : cLock : indVal : menu1 : menu2 : borda : [Blank] ),aviso]
    where cLock = cameraLock cl
          menu1 = desenhaSelection pics sel
          menu2 = desenhaSelection2 pics sel2
          cursorPic = desenhaCursorX sel (a,b) pics
          ref =  Scale 2.0 2.0 (selecionaPic pics 40)
          jogadorPic = desenhaJogador pics (Jogador player Este False)
          mapaPic = desenhaMapaSingle pics mapa
          indVal =  indValidado val
          borda = selecionaPic pics 41
          aviso = desenhaAviso ind1 ind2 pics

-- Desenha as intros
draw pics (Intro valor, jg , Editor pos s m (x,y) s2 c val cl)  = desenhaIntro pics valor

-- Desenha Info
draw pics (InfoTela , jg , ed)  = selecionaPic pics 85

----------------------------------------------------------------------
--                              Desenhar                            --
--                         Funções Auxiliares                       --
----------------------------------------------------------------------

-- undefined
draw _ _ = Blank

{-| Função que se encarrega de desenhar as intros dos niveis e do editor-}
desenhaIntro :: [Picture] -> Int -> Picture
desenhaIntro pics 1 = selecionaPic pics 68  -- Desenha a intro do editor
desenhaIntro pics 2 = selecionaPic pics 69  -- Desenha a intro do nivel 1 (Campanha)
desenhaIntro pics 3 = selecionaPic pics 70  -- Desenha a intro do nivel 2 (Campanha)
desenhaIntro pics 4 = selecionaPic pics 71  -- Desenha a intro do nivel 3 (Campanha)
desenhaIntro pics 5 = selecionaPic pics 72  -- Desenha a intro do nivel 4 (Campanha)
desenhaIntro pics 6 = selecionaPic pics 73  -- Desenha a intro do nivel 5 (Campanha)
desenhaIntro pics 7 = selecionaPic pics 74  -- Desenha a intro do nivel 1 (Editor)
desenhaIntro pics 8 = selecionaPic pics 75  -- Desenha a intro do nivel 2 (Editor)
desenhaIntro pics 9 = selecionaPic pics 76  -- Desenha a intro do nivel 3 (Editor)
desenhaIntro pics 10 = selecionaPic pics 77 -- Desenha a intro do nivel 4 (Editor)
desenhaIntro pics 11 = selecionaPic pics 78 -- Desenha a intro do nivel 5 (Editor)
desenhaIntro pics 12 = selecionaPic pics 25 -- Desenha a intro do nivel 5 (Editor)
desenhaIntro pics _ = Blank

{-| Função que decide ,consoante o tamanho do mapa , se a camera do jogo será fixa na origem ou irá acompanhar o jogador-}
cameraNecessaria :: Coordenadas -> Mapa -> Coordenadas
cameraNecessaria coords mapa
    | maximumX (desconstroiMapa mapa) > 29 = cameraUpdate coords mapa
    | maximumY (desconstroiMapa mapa) > 16 = cameraUpdate coords mapa
    | otherwise = (0,0)

{-| Função que se encarrega de fazer com que a camera acompanhe o jogador-}
cameraUpdate :: Coordenadas -> Mapa -> Coordenadas
cameraUpdate (x,y) mapa
    | x >= metadeX && y >= metadeY = (-(x-metadeX),-(-y+metadeY))
    | x >= metadeX && y <= metadeY = (-(x-metadeX),-(-y+metadeY))
    | x <= metadeX && y >= metadeY = (-x+metadeX,  -(-y+metadeY))
    | x <= metadeX && y <= metadeY = (-x+metadeX,  -(-y+metadeY))
    | y >= metadeY && x >= metadeX = (-(x-metadeX),-(-y+metadeY))
    | y >= metadeY && x <= metadeX = (-x+metadeX,  -(-y+metadeY))
    | y <= metadeY && x >= metadeX = (-(x-metadeX),-(y-metadeY))
    | y <= metadeY && x <= metadeX = (-x+metadeX,  -(y-metadeY))
    | otherwise = (0,0)
    where metadeY = quot (maximumY (desconstroiMapa mapa)) 2
          metadeX = quot (maximumX (desconstroiMapa mapa)) 2

{-| Função que se encarrega de desenhar a indicação da dimensao do mapa (Menu de gravação/ carregamento do editor)-}
indicaDimensao :: [(Peca, Coordenadas)] -> Coordenadas -> Picture
indicaDimensao mapa (a,b) = Pictures [dimensoes,coordsInd]
    where dimensoes = Color white (Translate 200.0 (-380.0) (Scale 0.3 0.3 (Text ("Dimensoes: " ++ show (maximumX mapa+1) ++ " x " ++ show (maximumY mapa-1)))))
          coordsInd = Color white (Translate 200.0 (-450.0) (Scale 0.3 0.3 (Text ("Jogador: (" ++ show a ++ "," ++ show (b-1) ++ ")" ))))

{-| Função que se encarrega de desenhar os avisos do editor -}
desenhaAviso :: Int -> Int -> [Picture] -> Picture
desenhaAviso 1 1 pics = selecionaPic pics 60
desenhaAviso 1 2 pics = selecionaPic pics 61
desenhaAviso 2 1 pics = selecionaPic pics 62
desenhaAviso 3 1 pics = selecionaPic pics 63
desenhaAviso 3 2 pics = selecionaPic pics 64
desenhaAviso 3 3 pics = selecionaPic pics 65
desenhaAviso 3 4 pics = selecionaPic pics 66
desenhaAviso 3 5 pics = selecionaPic pics 67
desenhaAviso 3 6 pics = selecionaPic pics 79
desenhaAviso _ _ pics = Blank

{-| Função que se encarrega de desenhar os diferentes cursores do editor -}
desenhaCursorX :: Int -> Coordenadas -> [Picture] -> Picture
desenhaCursorX 1 (a,b) pics = moverPicture (selecionaPic pics 36) (fromIntegral a ,fromIntegral b)
desenhaCursorX 2 (a,b) pics = moverPicture (selecionaPic pics 37) (fromIntegral a ,fromIntegral b)
desenhaCursorX 3 (a,b) pics = moverPicture (selecionaPic pics 38) (fromIntegral a ,fromIntegral b)
desenhaCursorX 4 (a,b) pics = moverPicture (selecionaPic pics 39) (fromIntegral a ,fromIntegral b)
desenhaCursorX 5 (a,b) pics = moverPicture (selecionaPic pics 32) (fromIntegral a ,fromIntegral b)
desenhaCursorX _ _ pics = Blank

{-| Função que se encarrega de desenhar o estado do camera lock do editor -}
cameraLock :: Bool -> Picture
cameraLock True  = Color white (Translate 500.0 (-500.0) (Scale 0.2 0.2 (Text "Camera Lock: ON")))
cameraLock False = Color white (Translate 500.0 (-500.0) (Scale 0.2 0.2 (Text "Camera Lock: OFF")))

{-| Função que se encarrega de desenhar o estado de validação do mapa do editor -}
indValidado :: Bool -> Picture
indValidado False = Color white (Translate 500.0 (-470.0) (Scale 0.2 0.2 (Text "Mapa validado: Nao")))
indValidado True  = Color white (Translate 500.0 (-470.0) (Scale 0.2 0.2 (Text "Mapa validado: Sim")))

{-| Função que desenha a marca de água boxbot -}
boxBotMarca :: Picture
boxBotMarca = Color white (Translate (-920.0) 500.0 (Scale 0.2 0.2 (Text "BoxBot")))

{-| Função que se encarrega de desenhar a indicação do nivel no canto do jogo -}
nivelMarca :: Int -> Picture
nivelMarca 1 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 1 (Facil)")))
nivelMarca 2 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 2 (Facil)")))
nivelMarca 3 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 3 (Medio)")))
nivelMarca 4 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 4 (Dificil)")))
nivelMarca 5 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 5 (Dificil)")))
nivelMarca 6 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 1 (Editor)")))
nivelMarca 7 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 2 (Editor)")))
nivelMarca 8 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 3 (Editor)")))
nivelMarca 9 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 4 (Editor)")))
nivelMarca 10 = Color white (Translate 700.0 500.0 (Scale 0.2 0.2 (Text "Nivel 5 (Editor)")))
nivelMarca _ = Blank

{-| Função que se encarrega de desenhar o mapa -}
desenhaMapaSingle :: [Picture] -> [(Peca, Coordenadas)] -> Picture
desenhaMapaSingle l mapa = Pictures (desenhaMapa l mapa)

{-| Função auxiliar do desenhaMapaSingle -}
desenhaMapa :: [Picture] -> [(Peca, Coordenadas)] -> [Picture]
desenhaMapa _ [] = []
desenhaMapa l ((p,(x,y)):ys)
    | p == Caixa = moverPicture (selecionaPic l 0) (x1,x2) : desenhaMapa l ys
    | p == Bloco = moverPicture (selecionaPic l 1) (x1,x2) : desenhaMapa l ys
    | p == Porta = moverPicture (selecionaPic l 2) (x1,x2) : desenhaMapa l ys
    | otherwise = desenhaMapa l ys
    where (x1,x2) = (fromIntegral x :: Float ,fromIntegral y :: Float)

{-| Função que se encarrega de desenhar o jogador-}
desenhaJogador :: [Picture] -> Jogador -> Picture
desenhaJogador pics (Jogador (a,b) dir False) = case dir of Oeste -> moverPicture (selecionaPic pics 3) (a2,b2)
                                                            Este  -> moverPicture (selecionaPic pics 4) (a2,b2)
                                                            where (a2,b2) = (fromIntegral a :: Float ,fromIntegral b :: Float)

-- Desenhar jogador com caixa (Possui ajuste de imagem)
desenhaJogador pics (Jogador (a,b) dir True) = case dir of Oeste -> Translate 0 32 (moverPicture (selecionaPic pics 5) (a2,b2))
                                                           Este  -> Translate 0 32 (moverPicture (selecionaPic pics 6) (a2,b2))
                                                           where (a2,b2) = (fromIntegral a :: Float ,fromIntegral b :: Float)

{-| Função que se encarrega de desenhar o menu de peças do editor -}
desenhaSelection :: [Picture] -> Int -> Picture
desenhaSelection pics v
    | v == 1 = Scale 3 3 (Translate (-275) 0 (selecionaPic pics 27))
    | v == 2 = Scale 3 3 (Translate (-275) 0 (selecionaPic pics 28))
    | v == 3 = Scale 3 3 (Translate (-275) 0 (selecionaPic pics 30))
    | v == 4 = Scale 3 3 (Translate (-275) 0 (selecionaPic pics 29))
    | v == 5 = Scale 3 3 (Translate (-275) 0 (selecionaPic pics 31))
    | otherwise = Blank

{-| Função que se encarrega de desenhar as intros dos niveis e do editor -}
desenhaSelection2 :: [Picture] -> Int -> Picture
desenhaSelection2 pics v
    | v == 1 = Scale 4 4 (Translate 0 (-115) (selecionaPic pics 33))
    | v == 2 = Scale 4 4 (Translate 0 (-115) (selecionaPic pics 34))
    | v == 3 = Scale 4 4 (Translate 0 (-115) (selecionaPic pics 35))
    | v == 4 = Scale 4 4 (Translate 0 (-115) (selecionaPic pics 42))
    | v == 5 = Scale 4 4 (Translate 0 (-115) (selecionaPic pics 43))
    | otherwise = Blank

{-| Função que se encarrega de desenhar as opções do menu do editor (InEditorOps) -}
opDraw :: [Picture] -> Int -> Modo -> Picture
opDraw pics val modo
    | modo == Carregar && val == 1 = selecionaPic pics 47
    | modo == Carregar && val == 2 = selecionaPic pics 48
    | modo == Carregar && val == 3 = selecionaPic pics 49
    | modo == Carregar && val == 4 = selecionaPic pics 80
    | modo == Carregar && val == 5 = selecionaPic pics 81
    | modo == Gravar && val == 1 = selecionaPic pics 50
    | modo == Gravar && val == 2 = selecionaPic pics 51
    | modo == Gravar && val == 3 = selecionaPic pics 52
    | modo == Gravar && val == 4 = selecionaPic pics 82
    | modo == Gravar && val == 5 = selecionaPic pics 83
    | otherwise = blank

{-| Função que se encarrega de ajustar as imagens do mapa -}
moverPicture :: Picture -> (Float,Float) -> Picture
moverPicture pic (x,y) = Scale 2 2 (Translate (x*32) (y*(-32)) pic)

-- | Função responsável por fazer com que as imagens fiquem corretamente espaçadas umas das outras,
ajustarMapa :: Picture -> (Int,Int)-> Picture
ajustarMapa mapa (a,b) = Translate (-(c/2*64)) (d/2*64) mapa
    where (c,d) = (fromIntegral a, fromIntegral b)

-- | Dimensão máxima X do mapa
maxX :: [Peca] -> Int
maxX = foldr (\ x -> (+) 1) 0

-- | Dimensão máxima Y do mapa
maxY :: Mapa -> Int
maxY = foldr (\ x -> (+) 1) 0


-----------------------------------------------------------------------------------------------
------------------------------------------  Eventos  ------------------------------------------ 
-----------------------------------------------------------------------------------------------

-- | Função que tranforma o tipo World dado pela função 'event' em IO World
eventIO :: Event -> World -> IO World

-----------------------------------------------------------------------
--                    Eventos IO do menu principal                   --
-----------------------------------------------------------------------

-- menu inicial -> Sair do jogo
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Sair n , jogo , e) = do putStrLn "Jogo terminado com sucesso"
                                                                                       exitSuccess
-- menu inicial -> load game
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial LoadGame n , jogo , e) =
    do input <- readFile "recursos/saves/game/save1.txt"
       if input == "" then return (MenuInicial LoadGame n , jogo , e) else return (ModoJogo n, stringToJogo input, e)

-- menu inicial (2) -> load game
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial2 LoadGame n , jogo , e) =
    do input <- readFile "recursos/saves/game/save1.txt"
       if input == "" then return (MenuInicial LoadGame n , jogo , e) else return (ModoJogo n, stringToJogo input, e)

-----------------------------------------------------------------------
--                   Eventos IO do menu de pausa/jogo                --
-----------------------------------------------------------------------

-- game menu -> sair do jogo
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo Sair n, jogo , e) = do putStrLn "Jogo terminado com sucesso"
                                                                                   exitSuccess
-- game menu -> save game 
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo SaveGame n , jogo , e) =
    do writeFile "recursos/saves/game/save1.txt" (show jogo)     -- grava jogo em ficheiro ao pressionar tecla "Space"
       return (Save n, jogo,e)                                  -- suspende jogo (só para ilustrar - não é necessário para o writeFile)


-----------------------------------------------------------------------
--                        Eventos IO da intro                        --
--                   (Carregamento do nivel seguinte)                --
-----------------------------------------------------------------------

-- Modo de jogo intro do nivel
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 2 , jg , ed)  =
    do input <- readFile "recursos/saves/game/nivel1.txt"
       return (ModoJogo 1,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 3 , jg , ed)  =
    do input <- readFile "recursos/saves/game/nivel2.txt"
       return (ModoJogo 2,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 4 , jg , ed)  =
    do input <- readFile "recursos/saves/game/nivel3.txt"
       return (ModoJogo 3,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 5 , jg , ed)  =
    do input <- readFile "recursos/saves/game/nivel4.txt"
       return (ModoJogo 4,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 6 , jg , ed)  =
    do input <- readFile "recursos/saves/game/nivel5.txt"
       return (ModoJogo 5,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 7 , jg , ed)  =
    do input <- readFile "recursos/saves/editor/save1e.txt"
       return (ModoJogo 6,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 8 , jg , ed)  =
    do input <- readFile "recursos/saves/editor/save2e.txt"
       return (ModoJogo 7,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 9 , jg , ed)  =
    do input <- readFile "recursos/saves/editor/save3e.txt"
       return (ModoJogo 8,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 10 , jg , ed)  =
    do input <- readFile "recursos/saves/editor/save4e.txt"
       return (ModoJogo 9,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 11 , jg , ed)  =
    do input <- readFile "recursos/saves/editor/save5e.txt"
       return (ModoJogo 10,stringToJogo input,ed )

-----------------------------------------------------------------------
--         Eventos IO da opção recomeçar do menu de jogo/pausa       --
--                         (Recarregar o nivel)                      --
-----------------------------------------------------------------------

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 1, jogo , ed)  =
    do input <- readFile "recursos/saves/game/nivel1.txt"
       return (ModoJogo 1,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 2, jogo , ed)  =
    do input <- readFile "recursos/saves/game/nivel2.txt"
       return (ModoJogo 2,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 3, jogo , ed)  =
    do input <- readFile "recursos/saves/game/nivel3.txt"
       return (ModoJogo 3,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 4, jogo , ed)  =
    do input <- readFile "recursos/saves/game/nivel4.txt"
       return (ModoJogo 4,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 5, jogo , ed)  =
    do input <- readFile "recursos/saves/game/nivel5.txt"
       return (ModoJogo 5,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 6, jogo , ed)  =
    do input <- readFile "recursos/saves/editor/save1e.txt"
       return (ModoJogo 6,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 7, jogo , ed)  =
    do input <- readFile "recursos/saves/editor/save2e.txt"
       return (ModoJogo 7,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 8, jogo , ed)  =
    do input <- readFile "recursos/saves/editor/save3e.txt"
       return (ModoJogo 8,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 9, jogo , ed)  =
    do input <- readFile "recursos/saves/editor/save4e.txt"
       return (ModoJogo 9,stringToJogo input,ed )

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo RestartJogo 10, jogo , ed)  =
    do input <- readFile "recursos/saves/editor/save5e.txt"
       return (ModoJogo 10,stringToJogo input,ed )

-- Menu 2
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial2 Restart n , jogo , e) =
    do input <- readFile "recursos/saves/game/nivel1.txt"
       return (ModoJogo 1,stringToJogo input,e )

-----------------------------------------------------------------------
--              Eventos IO do menu de gravação do editor             --
--                     (Gravar niveis do editor)                     --
-----------------------------------------------------------------------

-- Editor menu save
eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Gravar 1 coords, jg , Editor pos sel mapa (a,b) s2 camera val cl) =
    do writeFile "recursos/saves/editor/save1e.txt" (show (Jogo (adicionaVazioLinha (constroiMapa (tornaValido mapa))) (Jogador (tornaValidoJogador (a,b) mapa) Este False)))   -- grava jogo em ficheiro ao pressionar tecla "Space"
       return (InEditor , jg , Editor pos sel mapa (a,b) s2 camera val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Gravar 2 coords, jg , Editor pos sel mapa (a,b) s2 camera val cl) =
    do writeFile "recursos/saves/editor/save2e.txt" (show (Jogo (adicionaVazioLinha (constroiMapa (tornaValido mapa))) (Jogador (tornaValidoJogador (a,b) mapa) Este False)))      -- grava jogo em ficheiro ao pressionar tecla "Space"
       return (InEditor , jg , Editor pos sel mapa (a,b) s2 camera val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Gravar 3 coords, jg , Editor pos sel mapa (a,b) s2 camera val cl) =
    do writeFile "recursos/saves/editor/save3e.txt" (show (Jogo (adicionaVazioLinha (constroiMapa (tornaValido mapa))) (Jogador (tornaValidoJogador (a,b) mapa) Este False)))  -- grava jogo em ficheiro ao pressionar tecla "Space"
       return (InEditor , jg , Editor pos sel mapa (a,b) s2 camera val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Gravar 4 coords, jg , Editor pos sel mapa (a,b) s2 camera val cl) =
    do writeFile "recursos/saves/editor/save4e.txt" (show (Jogo (adicionaVazioLinha (constroiMapa (tornaValido mapa))) (Jogador (tornaValidoJogador (a,b) mapa) Este False)))  -- grava jogo em ficheiro ao pressionar tecla "Space"
       return (InEditor , jg , Editor pos sel mapa (a,b) s2 camera val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Gravar 5 coords, jg , Editor pos sel mapa (a,b) s2 camera val cl) =
    do writeFile "recursos/saves/editor/save5e.txt" (show (Jogo (adicionaVazioLinha (constroiMapa (tornaValido mapa))) (Jogador (tornaValidoJogador (a,b) mapa) Este False)))  -- grava jogo em ficheiro ao pressionar tecla "Space"
       return (InEditor , jg , Editor pos sel mapa (a,b) s2 camera val cl)

-----------------------------------------------------------------------
--              Eventos IO do menu de carregamento do editor         --
--                     (Carrega niveis do editor)                    --
-----------------------------------------------------------------------

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 1 1 , jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save1e.txt"
       return (InEditorOps Carregar 1 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Carregar 1 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save1e.txt"
       return (InEditor, jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Carregar 2 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save2e.txt"
       return (InEditor, jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Carregar 3 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save3e.txt"
       return (InEditor, jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input))) (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Carregar 4 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save4e.txt"
       return (InEditor, jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input))) (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyEnter) Down _ _) (InEditorOps Carregar 5 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save5e.txt"
       return (InEditor, jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input))) (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

-----------------------------------------------------------------------
--                  Eventos Do menu de carregamento                  -- 
--              (Carregar a preview do nivel do editor)              --
-----------------------------------------------------------------------

-- KEY DOWN

eventIO (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Carregar 1 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save2e.txt"
       return (InEditorOps Carregar 2 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Carregar 2 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save3e.txt"
       return (InEditorOps Carregar 3 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Carregar 3 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save4e.txt"
       return (InEditorOps Carregar 4 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Carregar 4 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save5e.txt"
       return (InEditorOps Carregar 5 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Carregar 5 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save1e.txt"
       return (InEditorOps Carregar 1 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

-- KEY UP

eventIO (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Carregar 1 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save5e.txt"
       return (InEditorOps Carregar 5 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Carregar 2 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save1e.txt"
       return (InEditorOps Carregar 1 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Carregar 3 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save2e.txt"
       return (InEditorOps Carregar 2 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Carregar 4 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save3e.txt"
       return (InEditorOps Carregar 3 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

eventIO (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Carregar 5 coords, jg , Editor pos sel mapa player s2 camera val cl)  =
    do input <- readFile "recursos/saves/editor/save4e.txt"
       return (InEditorOps Carregar 4 (getCoords(stringToJogo input)), jg , Editor (getCoords(stringToJogo input)) sel (desconstroiMapa (getMapa(stringToJogo input)))  (getCoords(stringToJogo input)) s2 (getCoordsFlip(stringToJogo input)) val cl)

-----------------------------------------------------------------------
--                      Eventos do load / save                       -- 
-----------------------------------------------------------------------

-- game menu -> save screen
eventIO  (EventKey (SpecialKey KeySpace) Down _ _) (Save n, jogo,e) = return (ModoJogo n, jogo, e)           -- retoma jogo  ao pressionar tecla "Space" 
eventIO  (EventKey (SpecialKey KeyEnter) Down _ _) (Save n, jogo,e) = return (MenuJogo SaveGame n, jogo, e)  -- retoma o menu jogo ao pressionar tecla "enter"

eventIO ev world = return (event ev world)

-----------------------------------------------------------------------
--                  Funções auxiliares dos eventIO                   -- 
-----------------------------------------------------------------------

{-| Função que dado um joo devolve apenas as coordenadas do jogador (mas com a coordenada x invertida) -}
getCoordsFlip :: Jogo -> Coordenadas
getCoordsFlip (Jogo mapa (Jogador (a,b) dir v)) = (-a,b)

{-| Função que dado um jogo devolve apenas as coordenadas do jogador -}
getCoords :: Jogo -> Coordenadas
getCoords (Jogo mapa (Jogador (a,b) dir v)) = (a,b)

{-| Função que dado um jogo devolve apenas o mapa -}
getMapa :: Jogo -> Mapa
getMapa (Jogo mapa (Jogador (a,b) dir v)) = mapa


-------------------------------------------------------------------------------------
--                                 Fim dos eventIO                                 --
-------------------------------------------------------------------------------------
--                                Inicio dos events                                --
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--                              Eventos Menu Principal                             --
-------------------------------------------------------------------------------------

-- | Função que reage ao teclado, um evento, tranformando o tipo World
event :: Event -> World -> World


-- Menu Principal -> Opção (Jogar)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuInicial Jogar n , jogo , e) = (MenuInicial SelecionarNivel n  , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial Jogar n , jogo , e) = (MenuInicial LoadGame n , jogo , e)

-- Menu Principal -> Opção (Load)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuInicial LoadGame n , jogo , e) = (MenuInicial Info n , jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial LoadGame n , jogo , e) = (MenuInicial Jogar n , jogo , e)

-- Menu Principal -> Opção (Selecionar Nivel)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial SelecionarNivel n , jogo , e) = (MenuNiveis Level1 n, jogo , e)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuInicial SelecionarNivel n , jogo , e) = (MenuInicial EditorOp n , jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuInicial SelecionarNivel n , jogo , e) = (MenuInicial Jogar n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial SelecionarNivel n , jogo , e) = (MenuInicial Info n , jogo , e)

-- Menu Principal -> Opção (Editor)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial EditorOp n , jogo , e) = (Intro 1, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuInicial EditorOp n , jogo , e) = (MenuInicial SelecionarNivel n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial EditorOp n , jogo , e) = (MenuInicial Sair n , jogo , e)

-- Menu Principal -> Opção (Sair)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial Sair n , jogo , e) = (MenuInicial EditorOp n , jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuInicial Sair n , jogo , e) = (MenuInicial Info n , jogo , e)

-- Menu Principal -> Opção (Info)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Info n , jogo , e) = (InfoTela, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial Info n , jogo , e) = (MenuInicial SelecionarNivel n , jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuInicial Info n , jogo , e) = (MenuInicial LoadGame n , jogo , e)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuInicial Info n , jogo , e) = (MenuInicial Sair n , jogo , e)

-- Sair do info tela
event (EventKey (SpecialKey KeyEnter) Down _ _) (InfoTela , jogo , e) = (MenuInicial Jogar 1, jogo , e)

-------------------------------------------------------------------------------------
--                                    Eventos Menu 2                               --
-------------------------------------------------------------------------------------

-- Menu 2 -> Opção (Continuar)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial2 Continuar n , jogo , e) = (MenuJogo ReturnMainMenu n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial2 Continuar n , jogo , e) = (MenuInicial2 Sair n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial2 Continuar n , jogo , e) = (MenuInicial2 Restart n , jogo , e)
event (EventKey (SpecialKey KeyRight) Down _ _)  (MenuInicial2 Continuar n , jogo , e) = (MenuInicial2 LoadGame n , jogo , e)

-- Menu 2 -> Opção (Restart)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial2 Restart n , jogo , e) = (MenuInicial2 Continuar n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial2 Restart n , jogo , e) = (MenuInicial2 SelecionarNivel n , jogo , e)

-- Menu 2 -> Opção (Load)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial2 LoadGame n , jogo , e) = (MenuInicial2 Continuar n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial2 LoadGame n , jogo , e) = (MenuInicial2 Restart n , jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuInicial2 LoadGame n , jogo , e) = (MenuInicial2 Continuar n , jogo , e)

-- Menu 2 -> Opção (Selecionar Nivel)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial2 SelecionarNivel n , jogo , e) = (MenuNiveis Level1 n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial2 SelecionarNivel n , jogo , e) = (MenuInicial2 Restart n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial2 SelecionarNivel n , jogo , e) = (MenuInicial2 EditorOp n , jogo , e)

-- Menu 2 -> Opção (Editor)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial2 EditorOp n , jogo , e) = (InEditor, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial2 EditorOp n , jogo , e) = (MenuInicial2 SelecionarNivel n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial2 EditorOp n , jogo , e) = (MenuInicial2 Sair n , jogo , e)

-- Menu 2 -> Opção (Sair)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial2 Sair n , jogo , e) = (MenuInicial Jogar 1 , jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuInicial2 Sair n , jogo , e) = (MenuInicial2 EditorOp n , jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuInicial2 Sair n , jogo , e) = (MenuInicial2 Continuar n , jogo , e)

-------------------------------------------------------------------------------------
--                        Eventos Menu de seleção de niveis                        --
-------------------------------------------------------------------------------------

-- Clicar enter com nivel selecionado
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level1 n, jogo , ed)  = (Intro 2, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level2 n, jogo , ed)  = (Intro 3, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level3 n, jogo , ed)  = (Intro 4, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level4 n, jogo , ed)  = (Intro 5, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level5 n, jogo , ed)  = (Intro 6, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level1e n, jogo , ed)  = (Intro 7, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level2e n, jogo , ed)  = (Intro 8, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level3e n, jogo , ed)  = (Intro 9, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level4e n, jogo , ed)  = (Intro 10, jogo ,ed )
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Level5e n, jogo , ed)  = (Intro 11, jogo ,ed )

-- Menu de seleção de niveis -> Opção (Nivel 1 campanha)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level1 n, jogo , e) = (MenuNiveis Level2 n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuNiveis Level1 n, jogo , e)  = (MenuNiveis Level1e n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 2 campanha)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level2 n, jogo , e) = (MenuNiveis Level3 n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level2 n, jogo , e)  = (MenuNiveis Level1 n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuNiveis Level2 n, jogo , e)  = (MenuNiveis Level2e n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 3 campanha)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level3 n, jogo , e) = (MenuNiveis Level4 n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level3 n, jogo , e) = (MenuNiveis Level2 n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuNiveis Level3 n, jogo , e) = (MenuNiveis Level3e n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 4 campanha)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level4 n, jogo , e) = (MenuNiveis Level5 n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level4 n, jogo , e)  = (MenuNiveis Level3 n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuNiveis Level4 n, jogo , e)  = (MenuNiveis Level4e n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 5 campanha)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level5 n, jogo , e) = (MenuNiveis Voltar n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level5 n, jogo , e) = (MenuNiveis Level4 n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuNiveis Level5 n, jogo , e) = (MenuNiveis Level5e n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 1 editor)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level1e n, jogo , e) = (MenuNiveis Level2e n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuNiveis Level1e n, jogo , e) = (MenuNiveis Level1 n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 2 editor)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level2e n, jogo , e) = (MenuNiveis Level3e n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level2e n, jogo , e) = (MenuNiveis Level1e n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuNiveis Level2e n, jogo , e) = (MenuNiveis Level2 n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 3 editor)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level3e n, jogo , e) = (MenuNiveis Level4e n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level3e n, jogo , e) = (MenuNiveis Level2e n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuNiveis Level3e n, jogo , e) = (MenuNiveis Level3 n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 4 editor)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level4e n, jogo , e) = (MenuNiveis Level5e n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level4e n, jogo , e) = (MenuNiveis Level3e n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuNiveis Level4e n, jogo , e) = (MenuNiveis Level4 n, jogo , e)

-- Menu de seleção de niveis -> Opção (Nivel 5 editor)
event (EventKey (SpecialKey KeyRight) Down _ _) (MenuNiveis Level5e n, jogo , e) =  (MenuNiveis Voltar n, jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (MenuNiveis Level5e n, jogo , e) =  (MenuNiveis Level4e n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuNiveis Level5e n, jogo , e) = (MenuNiveis Level5 n, jogo , e)

-- Menu de seleção de niveis -> Opção (Voltar)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNiveis Voltar n, jogo , e) = if n == 1 then (MenuInicial  SelecionarNivel n , jogo , e)
                                                                                            else (MenuInicial2 SelecionarNivel n , jogo , e)
event (EventKey (SpecialKey KeyLeft) Down _ _) (MenuNiveis Voltar n, jogo , e) = (MenuNiveis Level5 n, jogo , e)

-------------------------------------------------------------------------------------
--                             Eventos Menu de Pausa / Jogo                        --
-------------------------------------------------------------------------------------

-- Menu de seleção de jogo -> Opção (Continuar)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo ContinuarJogo n, jogo , e) = (ModoJogo n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuJogo ContinuarJogo n, jogo , e) = (MenuJogo Sair n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuJogo ContinuarJogo n, jogo , e) = (MenuJogo RestartJogo n, jogo , e)

-- Menu de seleção de jogo -> Opção (Restart)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuJogo RestartJogo n, jogo , e) = (MenuJogo ContinuarJogo n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuJogo RestartJogo n, jogo , e) = (MenuJogo SaveGame n, jogo , e)

-- Menu de seleção de jogo -> Opção (Gravar)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuJogo SaveGame n, jogo , e) = (MenuJogo RestartJogo n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuJogo SaveGame n, jogo , e) = (MenuJogo ReturnMainMenu n, jogo , e)

-- Menu de seleção de jogo -> Opção (Menu principal)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo ReturnMainMenu n, jogo , e) = (MenuInicial2 Continuar n , jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuJogo ReturnMainMenu n, jogo , e) = (MenuJogo SaveGame n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuJogo ReturnMainMenu n, jogo , e) = (MenuJogo Sair n, jogo , e)

-- Menu de seleção de jogo -> Opção (Sair)
event (EventKey (SpecialKey KeyUp) Down _ _)    (MenuJogo Sair n, jogo , e) = (MenuJogo ReturnMainMenu n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (MenuJogo Sair n, jogo , e) = (MenuJogo ContinuarJogo n, jogo , e)


-----------------------------------------------------------------------------------
--                              Eventos Modo de jogo                             -- 
-----------------------------------------------------------------------------------

event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Jogar n, jogo , ed)  = (Intro 2 , jogo , ed)
event (EventKey (SpecialKey KeyEsc) Down _ _)   (ModoJogo n, jogo , e)  = (MenuJogo ContinuarJogo n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (ModoJogo n, jogo , e)  = keyUp (ModoJogo n, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (ModoJogo n, jogo , e)  = (ModoJogo n, moveJogador jogo InterageCaixa , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (ModoJogo n, jogo , e)  = keyleft (ModoJogo n, jogo , e)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo n, jogo , e)  = keyRight (ModoJogo n, jogo , e)

event (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 12 , jg , ed)  = (MenuInicial Jogar 1, jg ,ed)

-----------------------------------------------------------------------
--                       Eventos Modo de Teste                       -- 
-----------------------------------------------------------------------

--event (EventKey (SpecialKey KeyEsc) Down _ _) (ModoJogoTeste, jogo , e) = (MenuJogo ContinuarJogo n, jogo , e)
event (EventKey (SpecialKey KeyUp) Down _ _)    (ModoJogoTeste, jogo , e) = keyUpTeste (ModoJogoTeste, jogo , e)
event (EventKey (SpecialKey KeyDown) Down _ _)  (ModoJogoTeste, jogo , e) = (ModoJogoTeste, moveJogador jogo InterageCaixa , e)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (ModoJogoTeste, jogo , e) = keyleftTeste (ModoJogoTeste, jogo , e)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogoTeste, jogo , e) = keyRightTeste (ModoJogoTeste, jogo , e)

-- Voltar ao editor
event (EventKey (SpecialKey KeyEsc) Down _ _)  (ModoJogoTeste, jogo , e) = (InEditor, jogo, e)

-- Vencer jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo, jogo , editor) = (MenuInicial Jogar 1, jogo , editor)


-----------------------------------------------------------------------------
--                               Eventos Editor                            -- 
-----------------------------------------------------------------------------

----------------------------
--  Coodenadas do cursor  --
----------------------------

-- Camera lock off
event (EventKey (SpecialKey KeyUp) Down _ _)    (InEditor, jg , Editor (a,b) sel m p s2 c val False) = (InEditor, jg , Editor (a,b-1) sel m p s2 c val False)
event (EventKey (SpecialKey KeyDown) Down _ _)  (InEditor, jg , Editor (a,b) sel m p s2 c val False) = (InEditor, jg , Editor (a,b+1) sel m p s2 c val False)
event (EventKey (SpecialKey KeyRight) Down _ _) (InEditor, jg , Editor (a,b) sel m p s2 c val False) = (InEditor, jg , Editor (a+1,b) sel m p s2 c val False)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (InEditor, jg , Editor (a,b) sel m p s2 c val False) = (InEditor, jg , Editor (a-1,b) sel m p s2 c val False)

-- Camera lock on
event (EventKey (SpecialKey KeyUp) Down _ _)    (InEditor, jg , Editor (a,b) sel m p s2 (x,y) val True) = (InEditor, jg , Editor (a,b-1) sel m p s2 (-a,b-1) val True)
event (EventKey (SpecialKey KeyDown) Down _ _)  (InEditor, jg , Editor (a,b) sel m p s2 (x,y) val True) = (InEditor, jg , Editor (a,b+1) sel m p s2 (-a,b+1) val True)
event (EventKey (SpecialKey KeyRight) Down _ _) (InEditor, jg , Editor (a,b) sel m p s2 (x,y) val True) = (InEditor, jg , Editor (a+1,b) sel m p s2 (-a-1,b) val True)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (InEditor, jg , Editor (a,b) sel m p s2 (x,y) val True) = (InEditor, jg , Editor (a-1,b) sel m p s2 (-a+1,b) val True)

----------------------------
--         Menu 1         --
----------------------------

---------- Alternar peça (menu 1)
event (EventKey (Char 'e') Down _ _) (InEditor, jg , Editor pos 1 m p s2 c val cl) = (InEditor, jg , Editor pos 2 m p s2 c val cl)
event (EventKey (Char 'e') Down _ _) (InEditor, jg , Editor pos 2 m p s2 c val cl) = (InEditor, jg , Editor pos 3 m p s2 c val cl)
event (EventKey (Char 'e') Down _ _) (InEditor, jg , Editor pos 3 m p s2 c val cl) = (InEditor, jg , Editor pos 4 m p s2 c val cl)
event (EventKey (Char 'e') Down _ _) (InEditor, jg , Editor pos 4 m p s2 c val cl) = (InEditor, jg , Editor pos 5 m p s2 c val cl)
event (EventKey (Char 'e') Down _ _) (InEditor, jg , Editor pos 5 m p s2 c val cl) = (InEditor, jg , Editor pos 1 m p s2 c val cl)

event (EventKey (Char 'E') Down _ _) (InEditor, jg , Editor pos 1 m p s2 c val cl) = (InEditor, jg , Editor pos 2 m p s2 c val cl)
event (EventKey (Char 'E') Down _ _) (InEditor, jg , Editor pos 2 m p s2 c val cl) = (InEditor, jg , Editor pos 3 m p s2 c val cl)
event (EventKey (Char 'E') Down _ _) (InEditor, jg , Editor pos 3 m p s2 c val cl) = (InEditor, jg , Editor pos 4 m p s2 c val cl)
event (EventKey (Char 'E') Down _ _) (InEditor, jg , Editor pos 4 m p s2 c val cl) = (InEditor, jg , Editor pos 5 m p s2 c val cl)
event (EventKey (Char 'E') Down _ _) (InEditor, jg , Editor pos 5 m p s2 c val cl) = (InEditor, jg , Editor pos 1 m p s2 c val cl)

---------- Alternar peça (menu 1) (voltar atras)
event (EventKey (Char 'q') Down _ _) (InEditor, jg , Editor pos 1 m p s2 c val cl) = (InEditor, jg , Editor pos 5 m p s2 c val cl)
event (EventKey (Char 'q') Down _ _) (InEditor, jg , Editor pos 2 m p s2 c val cl) = (InEditor, jg , Editor pos 1 m p s2 c val cl)
event (EventKey (Char 'q') Down _ _) (InEditor, jg , Editor pos 3 m p s2 c val cl) = (InEditor, jg , Editor pos 2 m p s2 c val cl)
event (EventKey (Char 'q') Down _ _) (InEditor, jg , Editor pos 4 m p s2 c val cl) = (InEditor, jg , Editor pos 3 m p s2 c val cl)
event (EventKey (Char 'q') Down _ _) (InEditor, jg , Editor pos 5 m p s2 c val cl) = (InEditor, jg , Editor pos 4 m p s2 c val cl)

event (EventKey (Char 'Q') Down _ _) (InEditor, jg , Editor pos 1 m p s2 c val cl) = (InEditor, jg , Editor pos 5 m p s2 c val cl)
event (EventKey (Char 'Q') Down _ _) (InEditor, jg , Editor pos 2 m p s2 c val cl) = (InEditor, jg , Editor pos 1 m p s2 c val cl)
event (EventKey (Char 'Q') Down _ _) (InEditor, jg , Editor pos 3 m p s2 c val cl) = (InEditor, jg , Editor pos 2 m p s2 c val cl)
event (EventKey (Char 'Q') Down _ _) (InEditor, jg , Editor pos 4 m p s2 c val cl) = (InEditor, jg , Editor pos 3 m p s2 c val cl)
event (EventKey (Char 'Q') Down _ _) (InEditor, jg , Editor pos 5 m p s2 c val cl) = (InEditor, jg , Editor pos 4 m p s2 c val cl)

---------- Colocar peça
event (EventKey (SpecialKey KeySpace ) Down _ _) (InEditor, jg , Editor (a,b) 1 mapa p s2 c _ cl) = (InEditor, jg , Editor (a,b) 1 (replacePeca (Bloco,(a,b)) mapa) p s2 c False cl)
event (EventKey (SpecialKey KeySpace ) Down _ _) (InEditor, jg , Editor (a,b) 2 mapa p s2 c _ cl) = (InEditor, jg , Editor (a,b) 2 (replacePeca (Caixa,(a,b)) mapa) p s2 c False cl)
event (EventKey (SpecialKey KeySpace ) Down _ _) (InEditor, jg , Editor (a,b) 4 mapa p s2 c _ cl) = (InEditor, jg , Editor (a,b) 4 (replacePeca (Porta,(a,b)) mapa) p s2 c False cl)

---------- Colocar jogador 
event (EventKey (SpecialKey KeySpace ) Down _ _) (InEditor, jg , Editor (a,b) 3 mapa player s2 c _ cl) = (InEditor, jg , Editor (a,b) 3 mapa (a,b) s2 c False cl)

---------- Eleminar peça 
event (EventKey (SpecialKey KeySpace ) Down _ _) (InEditor, jg , Editor (a,b) 5 mapa p s2 c _ cl) = (InEditor, jg , Editor (a,b) 5 (eleminaPeca (a,b) mapa) p s2 c False cl)

----------------------------
--         Menu 2         --
----------------------------

---------- Alternar opcao (menu 2)
event (EventKey (SpecialKey KeyTab) Down _ _) (InEditor, jg , Editor pos sel m p 1 c val cl) = (InEditor, jg , Editor pos sel m p 2 c val cl)
event (EventKey (SpecialKey KeyTab) Down _ _) (InEditor, jg , Editor pos sel m p 2 c val cl) = (InEditor, jg , Editor pos sel m p 3 c val cl)
event (EventKey (SpecialKey KeyTab) Down _ _) (InEditor, jg , Editor pos sel m p 3 c val cl) = (InEditor, jg , Editor pos sel m p 4 c val cl)
event (EventKey (SpecialKey KeyTab) Down _ _) (InEditor, jg , Editor pos sel m p 4 c val cl) = (InEditor, jg , Editor pos sel m p 5 c val cl)
event (EventKey (SpecialKey KeyTab) Down _ _) (InEditor, jg , Editor pos sel m p 5 c val cl) = (InEditor, jg , Editor pos sel m p 1 c val cl)

---------- Menu 2 eventos
event (EventKey (SpecialKey KeyEnter) Down _ _) (InEditor, jogo , Editor pos sel m p 1 c _ cl) = (avisoInvalido m, jogo , Editor pos sel m p 1 c (validaPotencialMapa  (tornaValido m)) cl)
event (EventKey (SpecialKey KeyEnter) Down _ _) (InEditor, jogo , Editor pos sel m p 2 c False cl)  = (Aviso 2 1, jogo , Editor pos sel m p 2 c False cl)
event (EventKey (SpecialKey KeyEnter) Down _ _) (InEditor, jogo , Editor pos sel m p 2 c True cl)  = (ModoJogoTeste, Jogo (adicionaVazioLinha (constroiMapa (tornaValido m))) (Jogador (tornaValidoJogador p m) Este False) , Editor pos sel m p 2 c True cl)
event (EventKey (SpecialKey KeyEnter) Down _ _) (InEditor, jogo , Editor pos sel m p 3 c val  cl)  = (MenuInicial Jogar 1 , jogo, editorInicio)

---------- Coodenadas da camera (camera lock off)
event (EventKey (Char 'w') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a,b-1) val False)
event (EventKey (Char 's') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a,b+1) val False)
event (EventKey (Char 'd') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a-1,b) val False)
event (EventKey (Char 'a') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a+1,b) val False)

event (EventKey (Char 'W') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a,b-1) val False)
event (EventKey (Char 'S') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a,b+1) val False)
event (EventKey (Char 'D') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a-1,b) val False)
event (EventKey (Char 'A') Down _ _) (InEditor, jg , Editor pos sel m p s2 (a,b) val False) = (InEditor, jg , Editor pos sel m p s2 (a+1,b) val False)

---------- Centrar 
event (EventKey (Char 'h') Down _ _) (InEditor, jg , Editor (x,y) sel m p s2 (a,b) val cl) = (InEditor, jg , Editor (-a,b) sel m p s2 (a,b) val cl)
event (EventKey (Char 'g') Down _ _) (InEditor, jg , Editor (x,y) sel m p s2 (a,b) val cl) = (InEditor, jg , Editor (x,y) sel m p s2 (-x,y) val cl)
event (EventKey (Char 'j') Down _ _) (InEditor, jg , Editor pos sel m (x,y) s2 (a,b) val cl) = (InEditor, jg , Editor pos sel m (x,y) s2 (-x,y) val cl) -- Centrar no jogador
event (EventKey (Char 'o') Down _ _) (InEditor, jg , Editor _ sel m p s2 _ val cl) = (InEditor, jg , Editor (0,0) sel m p s2 (0,0) val cl) -- centrar no origin

event (EventKey (Char 'H') Down _ _) (InEditor, jg , Editor (x,y) sel m p s2 (a,b) val cl) = (InEditor, jg , Editor (-a,b) sel m p s2 (a,b) val cl)
event (EventKey (Char 'G') Down _ _) (InEditor, jg , Editor (x,y) sel m p s2 (a,b) val cl) = (InEditor, jg , Editor (x,y) sel m p s2 (-x,y) val cl)
event (EventKey (Char 'J') Down _ _) (InEditor, jg , Editor pos sel m (x,y) s2 (a,b) val cl) = (InEditor, jg , Editor pos sel m (x,y) s2 (-x,y) val cl) -- Centrar no jogador
event (EventKey (Char 'O') Down _ _) (InEditor, jg , Editor _ sel m p s2 _ val cl) = (InEditor, jg , Editor (0,0) sel m p s2 (0,0) val cl) -- centrar no origin

---------- Camera lock change
event (EventKey (Char 'y') Down _ _) (InEditor, jg , Editor (x,y) sel m p s2 (a,b) val False) = (InEditor, jg , Editor (x,y) sel m p s2 (-x,y) val True)
event (EventKey (Char 'y') Down _ _) (InEditor, jg , Editor pos sel m p s2 c val True) = (InEditor, jg , Editor pos sel m p s2 c val False)

event (EventKey (Char 'Y') Down _ _) (InEditor, jg , Editor (x,y) sel m p s2 (a,b) val False) = (InEditor, jg , Editor (x,y) sel m p s2 (-x,y) val True)
event (EventKey (Char 'Y') Down _ _) (InEditor, jg , Editor pos sel m p s2 c val True) = (InEditor, jg , Editor pos sel m p s2 c val False)

---------- Limpar mapa
event (EventKey (SpecialKey KeyDelete) Down ctrl _) (InEditor, jg , Editor pos sel _ p s2 c val cl) = (InEditor, jg , Editor pos sel [] p s2 c val cl) --  elemina o mapa com combinação (control + delete)

-----------------------------------------------------------------------------
--                            Eventos Editor Opções                        -- 
-----------------------------------------------------------------------------

-- AVISO AO ENTRAR NO LOAD
event (EventKey (SpecialKey KeyEnter) Down _ _) (InEditor, jg , Editor pos sel m p 4 c val cl) = (Aviso 1 1 , jg , Editor pos sel m p 4 c val cl)
event (EventKey (SpecialKey KeyRight) Down _ _) (Aviso 1 1 , jg , ed) = (Aviso 1 2, jg , ed)
event (EventKey (SpecialKey KeyLeft) Down _ _)  (Aviso 1 2 , jg , ed) = (Aviso 1 1, jg , ed)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 1 2 , jg , Editor pos sel m p sel2 c val cl) = (InEditor, jg , Editor pos sel m p sel2 c val cl)

-- entrar no ops
event (EventKey (SpecialKey KeyEnter) Down _ _) (InEditor, jg , Editor pos sel m p 5 c True cl) = (InEditorOps Gravar 1 (0,0), jg , Editor pos sel m p 4 c True cl)
event (EventKey (SpecialKey KeyEnter) Down _ _) (InEditor, jg , Editor pos sel m p 5 c False cl) = (Aviso 3 6, jg , Editor pos sel m p 4 c False cl)

event (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Gravar 1 coords, jg , ed) = (InEditorOps Gravar 2 coords, jg , ed)
event (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Gravar 2 coords, jg , ed) = (InEditorOps Gravar 3 coords , jg , ed)
event (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Gravar 3 coords, jg , ed) = (InEditorOps Gravar 4 coords , jg , ed)
event (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Gravar 4 coords, jg , ed) = (InEditorOps Gravar 5 coords , jg , ed)
event (EventKey (SpecialKey KeyDown) Down _ _) (InEditorOps Gravar 5 coords, jg , ed) = (InEditorOps Gravar 1 coords , jg , ed)

event (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Gravar 1 coords, jg , ed) = (InEditorOps Gravar 5 coords, jg , ed)
event (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Gravar 2 coords, jg , ed) = (InEditorOps Gravar 1 coords, jg , ed)
event (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Gravar 3 coords, jg , ed) = (InEditorOps Gravar 2 coords, jg , ed)
event (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Gravar 4 coords, jg , ed) = (InEditorOps Gravar 3 coords, jg , ed)
event (EventKey (SpecialKey KeyUp) Down _ _) (InEditorOps Gravar 5 coords, jg , ed) = (InEditorOps Gravar 4 coords, jg , ed)

event (EventKey (Char 'S' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a,b+2), jg , ed)
event (EventKey (Char 'A' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a-2,b), jg , ed)
event (EventKey (Char 'W' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a,b-2), jg , ed)
event (EventKey (Char 'D' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a+2,b), jg , ed)
event (EventKey (Char 's' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a,b+2), jg , ed)
event (EventKey (Char 'a' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a-2,b), jg , ed)
event (EventKey (Char 'w' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a,b-2), jg , ed)
event (EventKey (Char 'd' ) Down _ _) (InEditorOps op v (a,b), jg , ed) = (InEditorOps op v (a+2,b), jg , ed)

-- AVISOS
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 2 1, jg , ed) = (InEditor,jg,ed)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 3 1, jg , ed) = (InEditor,jg,ed)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 3 2, jg , ed) = (InEditor,jg,ed)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 3 3, jg , ed) = (InEditor,jg,ed)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 3 4, jg , ed) = (InEditor,jg,ed)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 3 5, jg , ed) = (InEditor,jg,ed)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Aviso 3 6, jg , ed) = (InEditor,jg,ed)

-- INTROS
event (EventKey (SpecialKey KeyEnter) Down _ _) (Intro 1 , jogo , e) = (InEditor, jogo , e)

event _ s = s -- Restantes casos (Colocar no fim)

---------------------------------------------------------------------------------
--                              Funções auxiliares                             -- 
---------------------------------------------------------------------------------

{-| Função que encontra o motivo pelo qual o mapa é invalido e associa um aviso -}
avisoInvalido :: [(Peca, Coordenadas)] -> Menu
avisoInvalido mapa
    | soUmaPortaAux (tornaValido mapa) > 1 = Aviso 3 1
    | soUmaPortaAux (tornaValido mapa) == 0 = Aviso 3 2
    | not (caixasFlutuantes (tornaValido mapa)) = Aviso 3 3
    | not (existeChao  (tornaValido mapa)) = Aviso 3 4
    | otherwise = Aviso 3 5

{-| Elemina todas as peças numa dada coordenada e coloca lá uma determinada peça ou seja, impede a sobreposição de peças -}
replacePeca :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
replacePeca (p,coords) lista = (p,coords) : eleminaPeca coords lista

{-| Ajusta a posição do jogador consoante a função tornaValido (o +1, +2 é o ajuste do espaço vazio adicionado no topo do mapa) -}
tornaValidoJogador :: Coordenadas -> [(Peca,Coordenadas)] -> Coordenadas
tornaValidoJogador (x,y) mapa = (x-minimumX mapa,y-minimumY mapa+2)

{-| Remove coordenadas negativas de um mapa , fazendo com que a função constroiMapa funcione -}
tornaValido :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
tornaValido [] = []
tornaValido mapa = tornaValidoAux mapa mapa

{-| Função auxiliar do tornaValido -}
tornaValidoAux :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
tornaValidoAux [] _ = []
tornaValidoAux ((p,(x,y)):xs) listaRef = (p,(x-c,y-d)) : tornaValidoAux xs listaRef
    where c = minimumX listaRef
          d = minimumY listaRef

{-|  Adiciona duas linha de espaço vazio ao topo de um mapa, fazendo com que o jogador consiga passar no "teto" do mapa -}
adicionaVazioLinha :: Mapa -> Mapa
adicionaVazioLinha [] = []
adicionaVazioLinha mapa = adicionaVazioLinhaAux (maximumX (desconstroiMapa mapa)) : adicionaVazioLinhaAux (maximumX (desconstroiMapa mapa)) : mapa

{-| Função auxiliar do adicionaVazio -}
adicionaVazioLinhaAux :: Int -> [Peca]
adicionaVazioLinhaAux 0 = [Vazio]
adicionaVazioLinhaAux val = Vazio : adicionaVazioLinhaAux (val-1)

{- | Função que retorna o X minimo de um mapa que se encontra sobre a forma lista de pares -}
minimumX :: [(Peca, Coordenadas)] -> Int
minimumX = minimum . getXs

{- | Função que retorna o Y minimo de um mapa que se encontra sobre a forma lista de pares -}
minimumY :: [(Peca, Coordenadas)] -> Int
minimumY = minimum . getYs

{-|  Dada uma determinada coordenada e um mapa elemina todas as peças nessa dada coordenada -}
eleminaPeca :: Coordenadas -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
eleminaPeca _ [] = []
eleminaPeca (x,y) ((p,(a,b)):xs)
  | (x,y) == (a,b) = eleminaPeca (x,y) xs
  | otherwise = (p,(a,b)) : eleminaPeca (x,y) xs

{-| Determina se um determinado jogo está ganho com o movimento AndarEsquerda -}
keyleft :: World -> World
keyleft (ModoJogo n,jogo, editor) = if gameOver (moveJogador jogo AndarEsquerda)
                                    then (Intro (n+2),jogo, editor)
                                    else (ModoJogo n, moveJogador jogo AndarEsquerda , editor)
keyleft s = s

{-| Determina se um determinado jogo está ganho com o movimento AndarDireita -}
keyRight :: World -> World
keyRight (ModoJogo n,jogo, editor) = if gameOver (moveJogador jogo AndarDireita)
                                     then (Intro (n+2),jogo, editor)
                                     else (ModoJogo n, moveJogador jogo AndarDireita , editor)
keyRight s = s

{-| Determina se um determinado jogo está ganho com o movimento Trepar -}
keyUp :: World -> World
keyUp (ModoJogo n,jogo, editor) = if gameOver (moveJogador jogo Trepar)
                                  then (Intro (n+2),jogo, editor)
                                  else (ModoJogo n, moveJogador jogo Trepar , editor)
keyUp s = s

{-| Determina se um determinado teste de um jogo no editor está ganho com o movimento AndarEsquerda -}
keyleftTeste :: World -> World
keyleftTeste (ModoJogoTeste, jogo , editor) = if gameOver (moveJogador jogo AndarEsquerda)
                                              then (InEditor, jogo, editor)
                                              else (ModoJogoTeste, moveJogador jogo AndarEsquerda , editor)
keyleftTeste s = s

{-| Determina se um determinado teste de um jogo no editor está ganho com o movimento AndarDireita -}
keyRightTeste :: World -> World
keyRightTeste (ModoJogoTeste, jogo , editor) = if gameOver (moveJogador jogo AndarDireita)
                                               then (InEditor, jogo, editor)
                                               else (ModoJogoTeste, moveJogador jogo AndarDireita , editor)
keyRightTeste s = s

{-| Determina se um determinado teste de um jogo no editor está ganho com o movimento Trepar -}
keyUpTeste :: World -> World
keyUpTeste (ModoJogoTeste, jogo , editor) = if gameOver (moveJogador jogo Trepar)
                                            then (InEditor, jogo, editor)
                                            else (ModoJogoTeste, moveJogador jogo Trepar , editor)
keyUpTeste s = s




{- Funções que transformam a representação textual de um jogo (uma string), num jogo
Utiliza as funções auxiliares 'stringToMapa', 'stringToJogador', 'buscaTipoMaybe', 'delete' e 'constroiMapa'
-}
stringToJogo :: String -> Jogo
stringToJogo str = if buscaTipoMaybe (stringToMapa str) (a,b-1) == Just Caixa
                   then Jogo m1 (Jogador (a,b) dir True)
                   else Jogo m2 (Jogador (a,b) dir False)
                   where Just (Jogador (a,b) dir bl) = stringToJogador str
                         m1 = constroiMapa (delete (Caixa, (a,b-1)) (stringToMapa str))
                         m2 = constroiMapa (stringToMapa str)

-- | Função que aplica a função auxiliar 'stringToMapaAux' para transformar uma String num Mapa
stringToMapa :: String -> [(Peca, Coordenadas)]
stringToMapa str = stringToMapaAux str (0,0)

-- | Função auxiliar que transforma uma String num Mapa
stringToMapaAux :: String ->  (Int, Int) -> [(Peca, Coordenadas)]
stringToMapaAux [] _ = []
stringToMapaAux (h:t) (x,y) =
        case h of ' '  -> stringToMapaAux t (x+1,y)
                  'X'  -> (Bloco, (x,y)) : stringToMapaAux t (x+1,y)
                  'C'  -> (Caixa, (x,y)) : stringToMapaAux t (x+1,y)
                  'P'  -> (Porta, (x,y)) : stringToMapaAux t (x+1,y)
                  '\n' -> stringToMapaAux t (0,y+1)
                  _    -> stringToMapaAux t (x+1,y)

-- | Função que aplica a função auxiliar 'stringToJogadorAux' para transformar uma String num Jogador
stringToJogador :: String -> Maybe Jogador
stringToJogador str = stringToJogadorAux str (0,0)

-- | Função auxiliar que transforma uma String num Jogador
stringToJogadorAux :: String ->  (Int, Int) -> Maybe Jogador
stringToJogadorAux [] _ = Nothing
stringToJogadorAux (h:t) (x,y) =
        case h of ' '  -> stringToJogadorAux t (x+1,y)
                  'X'  -> stringToJogadorAux t (x+1,y)
                  'C'  -> stringToJogadorAux t (x+1,y)
                  'P'  -> stringToJogadorAux t (x+1,y)
                  '\n' -> stringToJogadorAux t (0,y+1)
                  '<'  -> Just (Jogador (x,y) Oeste False)
                  '>'  -> Just (Jogador (x,y) Este False)
                  _    -> stringToJogadorAux t (x+1,y)




-- | mapa do jogo predefinido
mapa1 :: Mapa  -- mapa 8x16
mapa1 = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
        ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
        ,[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
        ,[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
        ,[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
        ,[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
        ,[Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco]
        ,[Bloco,Porta,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Bloco]
        ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

-- | jogo predefinido
jogo1 :: Jogo
jogo1 = Jogo mapa1 (Jogador (13,7) Este False)