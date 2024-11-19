# PingPongAssemblyGame

A two-player classic PingPong game implemented in 8088 Assembly Language. The game features simple paddle controls, ball movement, scoring, and smooth termination. It is designed to run on DOS-based systems using the 8088 architecture.

## Features

- **Two-Player Mode:** Players control paddles to hit the ball and score points.
- **Ball Movement:** The ball moves diagonally and bounces off walls and paddles.
- **Paddle Controls:** Players can move their paddles left or right to block the ball.
- **Scoring System:** The first player to score 5 points wins the game.
- **Game Termination:** The game ends once a player reaches 5 points, and the program exits cleanly.

## Requirements

- 8088 Assembly Language
- DOSBox or an equivalent 8088 emulator for execution

## Instructions

1. **Compile and Run:**
   - Assemble the program using the `nasm` assembler (or any compatible assembler).
   - Run the resulting executable in a DOS environment (such as DOSBox).
   
2. **Gameplay:**
   - Player A controls the left paddle.
   - Player B controls the right paddle.
   - Use the **Left** and **Right** arrow keys to move the paddles.
   - The ball moves diagonally, bouncing off walls and paddles.
   - The game ends when a player's score reaches 5.

## Notes

- The game starts with the ball at Player B's side and moves diagonally towards Player A.
- The ball bounces back when it reaches the top (Player A's side) or bottom (Player B's side) of the screen.
- If a player fails to block the ball, the opponent scores a point, and the ball is reset to the starting position.
  


