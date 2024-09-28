# Jogo de Damas em Prolog

Este é um projeto de um jogo de damas desenvolvido em Prolog. Ele pode ser jogado no modo texto via terminal ou com uma interface gráfica usando HTML/CSS/JavaScript.

## Requisitos

- SWI-Prolog instalado ([Download SWI-Prolog](https://www.swi-prolog.org/download/stable))
- Navegador web moderno para a interface gráfica.

## Instalação

1. Clone o repositório:
   ```bash
   git clone https://github.com/seuusuario/jogo-de-damas-prolog.git
   cd jogo-de-damas-prolog

## Como Jogar (Modo Texto)

1. Iniciar o Prolog:
   ```bash
   swipl damas.pl
    
2. Iniciar o jogo:
   ```prolog
   ?- iniciar.

3. Escolha de Modo de Jogo

   - Jogador vs Maquina 
      ```prolog 
      1.

   - Maquina vs Maquina
      ```prolog
      2.

4. Movimentos
  
   - Movimentar uma peça:
      ```prolog
      mv(a3,b4).
   - Capturar uma peça:
      ```prolog
      cap(f4,[d6,b4]).

5. Para encerrar o jogo

   ```prolog
   halt.



