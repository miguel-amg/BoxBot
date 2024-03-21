# BoxBot 🤖📦
Projeto realizado no âmbito da cadeira de Laboratórios de Informática I. 

Este jogo é inspirado no jogo clássico **'Block dude'**, tendo este sido recriado em haskell com novas funcionalidades.

Em termos visuais foi mantida a identidade do 'Block dude' original, preservando o seu tema 'black and white'. 

Toda a arte foi desenvolvida pelo grupo sendo que nenhum asset foi obtido atráves de terceiros.

O projeto recebeu uma nota de **19/20**, sendo que a maioria dos objetivos estipulados pelos professores foram cumpridos.

# Lore
O BoxBot é um pequeno robô criado para arrumar caixas que se encontram em armazéns. 

Infelizmente com o passar dos anos a fábrica na qual o boxbot trabalhava ficou abandonada, tendo uma grande desorganização.

Ao fim de várias horas o pequeno robo tem de recarregar a sua bateria numa das estações de carregamento espalhadas pela fábrica.

A maioria das estações encontram-se em locais de difícil acesso, conseguirá o BoxBot sobreviver?

## Screenshots:
**Menu inicial**
![Imagem do menu Inicial](/Screenshots/Menu_Inicial.png)

**Menu de Níveis**
![Screenshot do menu de níveis](/Screenshots/Niveis.png)

**Nivel Fácil**
![Screenshot do nivel fácil](/Screenshots/Easy.png)

**Nivel Difícil**
![Screenshot do nivel difícil](/Screenshots/Hard.png)

**Editor de Mapas**
![Screenshot do menu de níveis](/Screenshots/Editor_Mapas.png)

**Menu de Pausa**
![Screenshot do menu de pausa](/Screenshots/Pausa.png)

**Menu de Informações**
![Screenshot do menu de informações](/Screenshots/Info.png)

# Gameplay
O gameplay do jogo é idêntico ao do block dude original:
- Seta cima: Pegar/Subir
- Seta esquerda: Andar esquerda
- Seta direita: Andar direita

# Funcionalidades
BoxBot é um jogo que possui funcionalidades novas que não estão disponíveis no jogo base:
- Uma campanha de 10 níveis com grau ascendente de dificuldade.
- 5 níveis da campanha editáveis (Permitindo ao utilizador criar a sua própria campanha)
- Editor de níveis completo: 
    - Hotkeys para facilitar a criação dos mapas
    - Sistema de armazenamento de niveis com pre-visualização.
- Possibilidade de partilhar níveis através de ficheiros de texto

# Compatibilidade e instalação
O jogo foi desenvolvido para Linux.

Para executar o jogo neste deverá ser executada a tarefa 5 com graphics gloss instalado.

Instalação das bibliotecas:
```
sudo apt-get install haskell-platform
cabal update
cabal install gloss
```

Uma versão para windows poderá ser desenvolvida no futuro.

## Realizado por:
-  Miguel Ângelo Martins Guimarães
-  Filipe Prudêncio Pacheco dos Santos
