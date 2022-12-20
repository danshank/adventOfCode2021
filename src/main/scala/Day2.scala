package Day2

import scala.io.Source

enum Movement:
    case Forward(n: Int)
    case Up(n: Int)
    case Down(n: Int)

def partOne(): Int =
    val filename = "src/main/scala/Day2.txt"
    try
        val rawInput = Source.fromFile(filename).getLines()
        val directions = rawInput.map(parseDirection)
        val initialCoordinate = (0, 0)
        val (x, y) = directions.foldLeft(initialCoordinate)(followDirection)
        x * y
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        0

def partTwo(): Int =
    val filename = "src/main/scala/Day2.txt"
    try
        val rawInput = Source.fromFile(filename).getLines()
        val directions = rawInput.map(parseDirection)
        val initialCoordinate = (0, 0, 0)
        val (x, y, _) = directions.foldLeft(initialCoordinate)(followDirectionAndAim)
        x * y
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        0    

def parseDirection(s: String): Movement =
    val directionComponents = s.split("\\s+")
    directionComponents(0) match
        case "forward" => Movement.Forward(directionComponents(1).toInt)
        case "up" => Movement.Up(directionComponents(1).toInt)
        case "down" => Movement.Down(directionComponents(1).toInt)
    
def followDirection(position: (Int, Int), direction: Movement): (Int, Int) =
    val (x, y) = position
    direction match
        case Movement.Forward(n) => (x + n, y)
        case Movement.Up(n) => (x, y - n)
        case Movement.Down(n) => (x, y + n)
    
def followDirectionAndAim(position: (Int, Int, Int), direction: Movement): (Int, Int, Int) =
    val (x, y, aim) = position
    direction match
        case Movement.Forward(n) => (x + n, y + (aim * n), aim)
        case Movement.Up(n) => (x, y, aim - n)
        case Movement.Down(n) => (x, y, aim + n)