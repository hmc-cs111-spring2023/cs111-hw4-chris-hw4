import scala.util.Random
import scala.io.StdIn.readLine

/** *****************************************************************************
  * Representing a game
  *
  * We will represent the board as a string of four characters. Each character
  * will be one of the following: B = Blue, Y = Yellow, R = Red, G = Green
  */
type Color = Char
type Board = String
val validColors = List('B', 'Y', 'R', 'G')

/** Get a random color from the list of valid colors */
def getRandomColor(): Color =
  validColors(scala.util.Random.between(0,4))

/** Given four colors, make a board from them */
def makeBoardFromColors(c1: Color, c2: Color, c3: Color, c4: Color): Board =
  s"$c1$c2$c3$c4"

/** Create a random board */
def getRandomBoard(): Board =
  makeBoardFromColors(getRandomColor(), getRandomColor(), getRandomColor(), getRandomColor())

/** Play one round of the game */
def playRound(board: Board): (Int, Int) = {

  // Get the four guesses from the user
  print("Enter a guess for spot 1: ")
  val spot1 = readLine()
  print("Enter a guess for spot 2: ")
  val spot2 = readLine()
  print("Enter a guess for spot 3: ")
  val spot3 = readLine()
  print("Enter a guess for spot 4: ")
  val spot4 = readLine()

  // Combine the guesses into a string
  var guess = s"$spot1$spot2$spot3$spot4"

  // Copy the board to a variable we can edit
  var answer = board

  var correctPlace = 0
  var correctColor = 0

  var guessLeftovers = ""
  var answerLeftovers = ""

  // Find all the guessed colors in matching places with the answer
  // Leftover colors get appended to strings, searched for colors in the wrong place later
  var ind = 0
  for (ind <- 0 until guess.length) {
      if (answer(ind) == guess(ind)) then 
        correctPlace += 1
      else 
        guessLeftovers += guess(ind)
        answerLeftovers += answer(ind)

  }

  // Search for matching colors in what's left of the guess and answer
  // Remove from both if a match is found!
  ind = 0
  for (ind <- 0 until guessLeftovers.length) {
      val checkInd = answerLeftovers.indexOf(guessLeftovers(0))
      if (checkInd != -1) then 
        correctColor += 1
        // Drop only the matching color in the answer using take and drop
        answerLeftovers = answerLeftovers.take(checkInd) + answerLeftovers.drop(checkInd+1)
      // Remove the color that we've checked
      guessLeftovers = guessLeftovers.substring(1)
      

  }

  (correctPlace, correctColor)
}


/** Score a guess
  *
  * A score is a tuple of two integers. The first integer is the number of
  * correct positions, and the second integer is the number of remaining correct
  * colors.
  */
def scoreGuess(board: Board, guess: Board): (Int, Int) = {

  // The initial score is (0, 0)
  var correctPositions = 0
  var correctColors = 0

  // Get the unique colors on the board
  val boardColors = board.toSet

  // Check each guess position against the corresponding board position
  // or (if there is not a match at that position) against the remainder of
  // the board.
  for (i <- 0 to 3) {
    if (guess(i) == board(i)) {
      correctPositions += 1
    } else if (boardColors.contains(guess(i))) {
      correctColors += 1
    }
  }

  (correctPositions, correctColors)
}

/** *****************************************************************************
  * Main program
  */

// When true, the program will print out the board at the start of the game
val DEBUG = true

@main
def mastermind() = {

  // Create a new board
  val board = getRandomBoard()

  if (DEBUG) {
    println(s"[DEBUG] The board is $board")
  }

  // Play rounds until the user guesses the board
  var score = (0, 0)
  while (score != (4, 0)) {
    score = playRound(board)
    val (correctPlace, correctColor) = score
    println(s"$correctPlace color(s) are in the correct place.")
    println(s"$correctColor color(s) are correct but in the wrong place.\n")
  }

  // End the game
  println(s"Congratulations! You figured out the board was $board")
}
