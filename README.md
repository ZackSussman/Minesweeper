# Minesweeper
Defines a context free grammar for manipulating a minesweeper board and an asscii art UI for live playing of the game inside the smlnj repl. 

1. Open minesweeper.sml in the smlnj repl
2. Type `evaluate "";` followed by a string which will contain the command you would like to enforce on the minesweeper board
3. valid commands are:

  n <i1> <i2>, create a new board of size i1xi1 using the seed i2 (i1 and i2 are integers)
  g <some string of characters containing wasd>, move around the board (w = up, a = left, s = down, d = right)
  r, reveal the value of the square you are currently on
  m, mark the current square as a bomb
