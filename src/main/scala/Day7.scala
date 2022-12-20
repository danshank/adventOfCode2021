package Day7
import scala.io.Source

def parseInput(): Array[Int] =
    val filename = "src/main/scala/Day7.txt"
    Source.fromFile(filename).getLines().next.split(",").map(s => s.toInt)

def optimalPosition(crabPositions: List[Int]): Int =
    val (lower, upper) = crabPositions.sorted.splitAt(crabPositions.size / 2)
    upper.head


def findMinimumFuelNeeded(crabPositions: List[Int]): Int =
    val k = optimalPosition(crabPositions)
    crabPositions.map(x => (x - k).abs).sum

def calculateFuelCost(k: Int, x: Int): Int =
    if k < x
    then (x - k + 1) * (x - k) / 2
    else (k - x + 1) * (k - x) / 2

def searchForOptimalPosition(k: Int, crabPositions: List[Int]): Int =
    val currentCost = crabPositions.map(x => calculateFuelCost(k, x)).sum
    println(s"Searching for position: $k, cost = $currentCost")
    val oneUpCost = crabPositions.map(x => calculateFuelCost(k + 1, x)).sum
    println(s"Searching for position: ${k + 1}, cost = $oneUpCost")
    if oneUpCost < currentCost
    then searchForOptimalPosition(k + 1, crabPositions)
    else
        val oneDownCost = crabPositions.map(x => calculateFuelCost(k - 1, x)).sum
        println(s"Searching for position: ${k - 1}, cost = $oneDownCost")
        if oneDownCost < currentCost
        then searchForOptimalPosition(k - 1, crabPositions)
        else currentCost

def findActualMinimumFuelNeeded(crabPositions: Array[Int]): Int =
    val x_m = crabPositions.sum / crabPositions.length
    searchForOptimalPosition(x_m, crabPositions.toList)
    
def partOne(): Int =
    try
        val crabPositions = parseInput()
        findMinimumFuelNeeded(crabPositions.toList)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def partTwo(): Int =
    try
        val crabPositions = parseInput()
        findActualMinimumFuelNeeded(crabPositions)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1
