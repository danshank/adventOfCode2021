package Day4

import scala.io.Source

case class BoardPosition(
    x: Int,
    y: Int,
    boardId: Int,
    played: Boolean
)

case class ParsedBoard(
    board: Vector[Vector[Int]]
)

case class PlayedValue(
    value: Int,
    played: Boolean
)

case class BingoGame(
    pulls: Vector[Int],
    pullMap: Map[Int, Set[BoardPosition]],
    boardMap: Map[Int, Vector[Vector[PlayedValue]]]
)

def newBingoGame(): BingoGame =
    BingoGame(Vector.empty, Map.empty[Int, Set[BoardPosition]], Map.empty[Int, Vector[Vector[PlayedValue]]])

def newParsedBoard(): ParsedBoard =
    ParsedBoard(Vector.fill(5, 5)(0))

def partOne(): Int =
    val filename = "src/main/scala/Day4.txt"
    try
        val rawInput = Source.fromFile(filename).getLines()
        val game = parseBingoGame(rawInput.toVector)
        playGame(game)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def partTwo(): Int =
    val filename = "src/main/scala/Day4.txt"
    try
        val rawInput = Source.fromFile(filename).getLines()
        val game = parseBingoGame(rawInput.toVector)
        val wonGames = Map.empty[Int, Int]
        val remainingGames = game.boardMap.keySet
        findLosingGame(game, wonGames, remainingGames)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def parseLine(state: (Int, Int, Int, ParsedBoard, BingoGame), value: Int) : (Int, Int, Int, ParsedBoard, BingoGame) =
    val (boardId, x, y, parsedBoard, game) = state
    val position = BoardPosition(x, y, boardId, false)
    val newPullMap = 
        if game.pullMap.contains(value)
        then 
            val newSet = game.pullMap(value) + position
            game.pullMap + (value -> newSet)
        else game.pullMap + (value -> Set.apply(position))    
    val newParsedBoard = parsedBoard.board.updated(x, parsedBoard.board(x).updated(y, value))
    (boardId, x, y + 1, ParsedBoard(newParsedBoard), BingoGame(game.pulls, newPullMap, game.boardMap))

def parseBoard(state: (Int, Int, Int, ParsedBoard, BingoGame), line: String): (Int, Int, Int, ParsedBoard, BingoGame) =
    val (boardId, x, y, parsedBoard, game) = state
    if line == ""
    then
        val freshlyParsedBoard = parsedBoard.board.map(row => row.map(entry => PlayedValue(entry, false))) 
        (boardId + 1, 0, 0, newParsedBoard(), BingoGame(game.pulls, game.pullMap, game.boardMap + (boardId -> freshlyParsedBoard)))
    else
        val (_, _, _, newParsedBoard, newGame) = line.split("\\s+").filter(entry => entry != "").map(s => s.toInt).foldLeft(state)(parseLine)
        (boardId, x + 1, 0, newParsedBoard, newGame)


def parseBingoGame(rawInput: Vector[String]): BingoGame =
    val pulls = rawInput.head.split(",").map(s => s.toInt)
    val initialState = (0, 0, 0, newParsedBoard(), newBingoGame())
    val (_, _, _, _, bingoGame) = rawInput.tail.tail.foldLeft(initialState)(parseBoard)
    BingoGame(pulls.toVector, bingoGame.pullMap, bingoGame.boardMap)

def lineHasWon(line: Vector[(Int, Int)], board: Vector[Vector[PlayedValue]]): (Int, Boolean) =
    val lineHasWon = line.foldLeft(true)((allArePlayedSoFar, coord) => {
        val (x, y) = coord
        allArePlayedSoFar && board(x)(y).played
    })
    if lineHasWon
    then
        val partialScore = board.flatten.foldLeft(0)((score, position) => {
            if position.played
            then score
            else score + position.value
        })
        (partialScore, true)
    else (0, false)

def boardHasWon(x: Int, y: Int, board: Vector[Vector[PlayedValue]]): (Int, Boolean) =
    val rowToCheck = Vector.range(0, 5).map(j => (x, j))
    val columnToCheck = Vector.range(0, 5).map(i => (i, y))
    val backSlash = Vector.range(0, 5).map(i => (i, i))
    val forwardSlash = Vector.range(0, 5).map(i => (i, 4 - i))
    val checkBackSlash = x == y
    val checkForwardSlash = (4 - x) == y
    val linesToCheck = (checkBackSlash, checkForwardSlash) match
        case (true, true) => Vector(rowToCheck, columnToCheck, backSlash, forwardSlash)
        case (true, false) => Vector(rowToCheck, columnToCheck, backSlash)
        case (false, true) => Vector(rowToCheck, columnToCheck, forwardSlash)
        case (false, false) => Vector(rowToCheck, columnToCheck)
    linesToCheck.foldLeft((0, false))((winCon, lineToCheck) => {
        val (score, boardWon) = winCon
        if (boardWon)
        then winCon
        else 
            val (partialScore, lineWins) = lineHasWon(lineToCheck, board)
            if lineWins
            then (partialScore * board(x)(y).value, lineWins)
            else (0, false)
    })
    
    
def updateBoardMap(pulls: Vector[BoardPosition], boardMap: Map[Int, Vector[Vector[PlayedValue]]]) : (Int, Boolean, Map[Int, Vector[Vector[PlayedValue]]]) =
    pulls.headOption match
        case head: Some[BoardPosition] =>
            val (boardId, x, y) = (head.value.boardId, head.value.x, head.value.y)
            val boardToPlay = boardMap(boardId)
            val positionToUpdate = boardToPlay(x)(y)
            val newBoard = boardToPlay.updated(x, boardToPlay(x).updated(y, PlayedValue(positionToUpdate.value, true)))
            val updatedBoardMap = boardMap + (boardId -> newBoard)
            val (score, boardWon) = boardHasWon(x, y, newBoard)
            if boardWon
            then (score, true, updatedBoardMap)
            else updateBoardMap(pulls.tail, updatedBoardMap)
        case None => (0, false, boardMap)

def findBoardsThatWon(wonBoards: Map[Int, Int], pulls: Vector[BoardPosition], boardMap: Map[Int, Vector[Vector[PlayedValue]]]): (Map[Int, Int], Map[Int, Vector[Vector[PlayedValue]]]) =
    pulls.headOption match
        case head: Some[BoardPosition] =>
            val (boardId, x, y) = (head.value.boardId, head.value.x, head.value.y)
            if (wonBoards.contains(boardId))
            then
                findBoardsThatWon(wonBoards, pulls.tail, boardMap)
            else
                val boardToPlay = boardMap(boardId)
                val positionToUpdate = boardToPlay(x)(y)
                val newBoard = boardToPlay.updated(x, boardToPlay(x).updated(y, PlayedValue(positionToUpdate.value, true)))
                val updatedBoardMap = boardMap + (boardId -> newBoard)
                val (score, boardWon) = boardHasWon(x, y, newBoard)
                if boardWon
                then
                    val newWonBoards = wonBoards + (boardId -> score)
                    findBoardsThatWon(newWonBoards, pulls.tail, updatedBoardMap)
                else
                    findBoardsThatWon(wonBoards, pulls.tail, updatedBoardMap)
        case None =>
            (wonBoards, boardMap)


def playGame(game: BingoGame): Int =
    val pull = game.pulls.head
    println(s"Pulled $pull")
    val pulledBoardPositions = game.pullMap(pull)
    val (score, gameWon, updatedBoardMap) = updateBoardMap(pulledBoardPositions.toVector, game.boardMap)
    if gameWon
    then score
    else playGame(BingoGame(game.pulls.tail, game.pullMap, updatedBoardMap))
    
def findLastGame(remainingBoards: Set[Int], boardIdsThatWon: Set[Int]): (Option[Int], Set[Int]) =
    boardIdsThatWon.headOption match
        case head: Some[Int] =>
            val boardId = head.value
            val newRemainingBoards = remainingBoards - boardId
            if newRemainingBoards.size == 0
            then (Some(boardId), newRemainingBoards)
            else findLastGame(newRemainingBoards, boardIdsThatWon.tail)
        case None => (None, remainingBoards)


def findLosingGame(game: BingoGame, wonBoards: Map[Int, Int], remainingBoards: Set[Int]) : Int =
    val pull = game.pulls.head
    println(s"Pulled $pull")
    //println(s"Remaining boards to win: $remainingBoards")
    val pulledBoardPositions = game.pullMap(pull)
    val (newlyWonBoards, updatedBoardMap) = findBoardsThatWon(wonBoards, pulledBoardPositions.toVector, game.boardMap)
    val boardIdsThatJustWon = newlyWonBoards.keySet.diff(wonBoards.keySet)
    println(s"boards that won: $boardIdsThatJustWon")
    val (lastBoardToWin, newlyRemainingBoards) = findLastGame(remainingBoards, boardIdsThatJustWon)
    lastBoardToWin match
        case boardId: Some[Int] =>
            println(s"Finding score for boardId ${boardId.value}")
            newlyWonBoards(boardId.value)
        case None => 
            val newBingoGame = BingoGame(game.pulls.tail, game.pullMap, updatedBoardMap)
            findLosingGame(newBingoGame, newlyWonBoards, newlyRemainingBoards)