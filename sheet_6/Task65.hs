import TicTacToe
{--
play tictactoe [5, 1, 9, 2, 3, 6, 7]
play tictactoe [5, 1, 9, 4, 6, 7]
play tictactoe [5, 1, 9, 4, 6, 8, 7, 3, 2]
--}
tictactoe = newGame "X" "O"
play = doTurns
