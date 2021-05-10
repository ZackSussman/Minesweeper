datatype squareValue = Indicator of int | Bomb
datatype status = Hidden | Revealed | Marked
type square = status * squareValue
type board = square list list * (int * int)
exception RevealedBomb
exception UserWins
exception GameOver

fun applyFuncToNth f n [] = []
  | applyFuncToNth f n (x::xs) = if n = 0 then (f x :: xs) else x::(applyFuncToNth f (n - 1) xs)

fun squareToString(Hidden, _) = "+"
  | squareToString(Marked, _) = "b"
  | squareToString(Revealed, Indicator (n)) = Int.toString n
  | squareToString(Revealed, Bomb) = "x"

fun boardToString([]) = ""
  | boardToString(row::rest) = let
                                    fun rowToString ([]) = "\n"
                                      | rowToString (s::sqs) = (squareToString s) ^ " " ^ rowToString(sqs)
                                in
                                     (rowToString row) ^ (boardToString rest)
                                end
  
fun printBoard(b, (x, y)) = let
                              val boardWithoutPosition = boardToString(b)
                            in
                              print(String.implode (applyFuncToNth (fn _ => #"_") (y*(2*List.length b) + y + 2*x) (String.explode boardWithoutPosition)))
                            end

fun makeBoard(seed, n) =  let
                            val gen = Random.rand (seed, seed*150)
                            fun makeN' 0 = [] | makeN' n = (n-1)::(makeN' (n-1))
                            val makeN = fn n => List.rev(makeN' n)
                            fun mkRow () = List.map (fn ~1 => (Hidden, Bomb) | idx => (Hidden, Indicator idx)) (List.map (fn idx => if Random.randRange (0, 4) gen = 0 then ~1 else idx) (makeN n))
                            val boardWithoutNumbers = List.map (fn idx => (idx, mkRow())) (makeN n)
                            fun get(x, y) = let val (idx, row) = List.nth(boardWithoutNumbers, y) in List.nth(row, x) end
                            fun countBombsNextToSquare(x, y) = let
                                                                  val isValidIndex = fn idx => case (Int.compare(idx, ~1), Int.compare(idx, n)) of (GREATER, LESS) => true | _ => false
                                                                  val indeciesToTry = List.filter (fn (x, y) => isValidIndex x andalso isValidIndex y) [(x - 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1), (x + 1, y - 1), (x - 1, y + 1)]
                                                                in
                                                                  List.foldr (op +) 0 (List.map (fn (x, y) => case get(x, y) of (_, Bomb) => 1 | _ => 0) indeciesToTry)
                                                                end
                            
                          in
                            (List.map (fn (y, row) => List.map (fn (r, Indicator x) => (r, Indicator (countBombsNextToSquare(x, y))) | b => b ) row) boardWithoutNumbers, (0, 0))
                          end

  fun revealSquare((b, (x, y))) = let
                                    fun reveal (_, Bomb) = raise RevealedBomb
                                      | reveal (_, v) = (Revealed, v)
                                  in
                                    (applyFuncToNth (fn row => applyFuncToNth reveal x row) y b, (x, y))
                                  end

  fun gameOver(win, (b, (x, y))) = let
                                      val revealedBoard = List.map (fn row => List.map (fn (_, v) => (Revealed, v)) row) b
                                   in 
                                      if win then let val len = List.length b val () = print("congradulations, you found all the bombs! The total board is: \n") val () = printBoard(revealedBoard, (len, len)) in () end
                                      else let val () = print("you revelead a bomb :(,\n") val () = printBoard(revealedBoard, (List.length b, List.length b)) in () end
                                   end

  fun isBoardCompleted((b, (x, y))) = let
                                        fun isSquareANonMarkedBomb (Marked, _) = false
                                          | isSquareANonMarkedBomb (_, Bomb) = true
                                          | isSquareANonMarkedBomb (_, _) = false
                                        val justBools = List.concat (List.map (fn row => List.map isSquareANonMarkedBomb row) b)
                                        exception ShortCircuit
                                      in
                                        not (List.exists (fn x => x) justBools)
                                      end

  fun changeMark((b, (x, y))) = let 
                                    fun change (Marked, v) = (Hidden, v)
                                      | change (Hidden, v) = (Marked, v)
                                      | change x = x
                                    val resultBoard = (applyFuncToNth (fn row => applyFuncToNth change x row) y b, (x, y))
                                    val () = if isBoardCompleted(resultBoard) then raise UserWins else ()
                                  in
                                    resultBoard
                                  end
                                  

(*terminals: g, w, a, s, d, m, r, n, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0*)
(*non terminals: N, D*)
(*N -> any string consisting of only numbers*)
(*D -> any string consisting of only wasd*)
(*S, start*)
(*rules:
  S -> "" empty string
  S -> gDS move in a direction
  S -> rS reveal a bomb
  S -> mS mark/unmark a bomb
  S -> nNNS start a new game on an num1xnum1 board made from seed num2
*)

datatype token = EMPTY | GOTO | REVEAL | MARK | NEW | DIR of string | NUM of string
datatype exp = Empty | Goto of (string * exp) | Reveal of exp | Mark of (exp) | New of (string * string * exp)
exception TokenError
exception Space
exception ParseError

fun tokenizePiece #"g" = GOTO
  | tokenizePiece #"r" = REVEAL
  | tokenizePiece #"m" = MARK
  | tokenizePiece #"n" = NEW
  | tokenizePiece #" " = raise Space

(*char list -> string * char list*)
fun tokenizeNum [] = ("", [])
  | tokenizeNum (c::cs) = case (Int.fromString (Char.toString c)) of 
                             SOME n => let val (restStr, restChrs) = tokenizeNum cs in ((Char.toString c) ^ restStr, restChrs) end
                            | NONE => ("", c::cs)

(*char list -> string * char list*)
fun tokenizeDir [] = ("", [])
  | tokenizeDir (c::cs) = case c of 
                            #"w" => let val (restStr, restChrs) = tokenizeDir cs in ("w" ^ restStr, restChrs) end
                          | #"a" => let val (restStr, restChrs) = tokenizeDir cs in ("a" ^ restStr, restChrs) end
                          | #"s" => let val (restStr, restChrs) = tokenizeDir cs in ("s" ^ restStr, restChrs) end
                          | #"d" => let val (restStr, restChrs) = tokenizeDir cs in ("d" ^ restStr, restChrs) end
                          | _ => ("", c::cs)

(*tokenize: char list -> token list*)
fun tokenize [] = [EMPTY]
  | tokenize (c::cs) = (tokenizePiece c)::(tokenize cs) 
                        handle Match => (case Int.fromString (Char.toString c) of 
                                          SOME n => let val (num, rest) = tokenizeNum(c::cs) in (NUM num)::(tokenize(rest)) end
                                        | NONE => let val (dir, rest) = tokenizeDir(c::cs) in if rest = (c::cs) then raise TokenError else (DIR dir)::(tokenize(rest)) end)
                             | Space => tokenize cs

fun userInputToTokens str = tokenize (String.explode str)


(*parse: token list -> exp*)
fun parse [EMPTY] = Empty
  | parse (GOTO::(DIR dir)::ts) = Goto(dir, parse ts)
  | parse (REVEAL::ts) = Reveal(parse ts)
  | parse (MARK::ts) = Mark(parse ts)
  | parse (NEW::(NUM size)::(NUM seed)::ts) = New(size, seed, parse ts)
  | parse _ = raise ParseError


val parseUserInput = parse o userInputToTokens

(*board -> exp -> board *)
fun evalExpOnBoard (b : board) Empty = b
  | evalExpOnBoard  (b, coord) (Goto(moves, rest)) = (let
                                            fun charToDir(#"a", (x, y)) = ((x - 1) mod List.length b, y)
                                              | charToDir(#"d", (x, y)) = ((x + 1) mod List.length b, y)
                                              | charToDir(#"s", (x, y)) = (x, (y + 1) mod List.length b)
                                              | charToDir(#"w", (x, y)) = (x, (y - 1) mod List.length b)
                                            fun evalMovesOnBoard(b, []) = b
                                              | evalMovesOnBoard((b, coord), c::rest) = evalMovesOnBoard((b, charToDir(c, coord)), rest)
                                          in
                                            evalExpOnBoard (evalMovesOnBoard((b, coord), String.explode(moves))) rest
                                          end)
  | evalExpOnBoard b (Reveal(rest)) = ((evalExpOnBoard (revealSquare b) rest) handle RevealedBomb => (let val () = (gameOver(false, b)) in raise GameOver end))
  | evalExpOnBoard b (Mark(rest)) = ((evalExpOnBoard (changeMark b) rest) handle UserWins => (let val () = (gameOver(true, b)) in raise GameOver end))
  | evalExpOnBoard b (New(n1, n2, rest)) = let val SOME n2i = Int.fromString n2 val SOME n1i = Int.fromString n1 in evalExpOnBoard (makeBoard(n2i, n1i)) rest end

val currentBoard = ref (makeBoard(1, 1))

fun evaluate input = let val () = currentBoard := (evalExpOnBoard (!currentBoard) (parseUserInput input)) val () = (printBoard (!currentBoard)) in () end  

