package Day9

import scala.io.Source

def parseInput(): Vector[Vector[Int]] =
    val filename = "src/main/scala/Day9.txt"
    Source.fromFile(filename).getLines().map(line => line.toCharArray.map(c => c.asDigit).toVector).toVector

def liftOfLift(i: Int, j: Int, heights: Vector[Vector[Int]]): Option[Int] =
    if i < 0 || j < 0
    then None
    else
        heights.lift(i) match
            case x: Some[Vector[Int]] =>
                x.value.lift(j)
            case None => None

def isLower(x: Option[Int], y: Option[Int]): Boolean =
    (x, y) match
        case (a: Some[Int], b: Some[Int]) =>
            a.value < b.value
        case (a: Some[Int], None) => true
        case _ => false

def calculateRiskFactor(i: Int, j: Int, heights: Vector[Vector[Int]]): Int =
    val adjacentCoords = List(
        liftOfLift(i, j-1, heights),
        liftOfLift(i, j+1, heights),
        liftOfLift(i-1, j, heights),
        liftOfLift(i+1, j, heights)
    )
    val height = liftOfLift(i, j, heights)
    val isLowest = adjacentCoords.foldLeft(true)((stillLower, adjacentCoord) => stillLower && isLower(height, adjacentCoord))
    isLowest match
        case true => 
            println(s"At coords ($i, $j), height ${height.get} is lowest")
            println(adjacentCoords)
            height.get + 1
        case false => 0


def findRiskFactor(heights: Vector[Vector[Int]]): Int =
    var riskFactor = 0
    for (i <- 0 to heights.length)
        for (j <- 0 to heights.head.length)
            riskFactor += calculateRiskFactor(i, j, heights)
    riskFactor

def partOne(): Int =
    try
        val heights = parseInput()
        findRiskFactor(heights)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def checkAdjacentCoordsInBasin(basins: Map[(Int, Int), Int], adjacentCoords: List[(Int, Int)]): Option[Int] =
    adjacentCoords.headOption match
        case coord: Some[(Int, Int)] =>
            if basins.contains(coord.value) then Some(basins(coord.value))
            else checkAdjacentCoordsInBasin(basins, adjacentCoords.tail)
        case None => None

def fillBasins(basinRegistry: (Map[(Int, Int), Int], Int), coord: (Int, Int), basinRidge: Set[(Int, Int)], heights: Vector[Vector[Int]]) : (Map[(Int, Int), Int], Int) =
    val (basins, basinIndex) = basinRegistry
    val (i, j) = coord
    if i < 0 || j < 0 || i >= heights.length || j >= heights.head.length then basinRegistry
    else if basinRidge.contains(coord) || basins.contains(coord) then basinRegistry
    else
        println(s"Checking coord $coord to see if it's in basin $basinIndex")        
        val adjacentCoords = List(
            (i, j+1),
            (i, j-1),
            (i+1, j),
            (i-1, j)
        )
        val updatedBasins = basins + (coord -> basinIndex)
        val (basinsWithAdjacent, _) = adjacentCoords.foldLeft((updatedBasins, basinIndex))((b, adjacentCoord) => fillBasins(b, adjacentCoord, basinRidge, heights))
        (basinsWithAdjacent, basinIndex)

def findBasinProduct(heights: Vector[Vector[Int]]): Int =
    val allCoords = heights.zipWithIndex.map((row, i) => row.zipWithIndex.map((_, j) => (i, j))).flatten
    // println(allCoords)
    val basinRidge = allCoords.filter((i, j) => heights(i)(j) == 9).toSet
    val basins = Map.empty[(Int, Int), Int]
    val basinIndex = 0
    val (mappedBasins, _) = allCoords.foldLeft((basins, basinIndex))((basinRegistry, coord) => {
        val (updatedBasins, prevIndex) = fillBasins(basinRegistry, coord, basinRidge, heights)
        (updatedBasins, prevIndex + 1)
    })
    // println(mappedBasins)
    val allBasinSizes = mappedBasins.values.groupBy(identity).mapValues(_.size).values.toVector.sorted
    val largestBasinSizes = allBasinSizes.takeRight(3)
    // println(allBasinSizes)
    // println(largestBasinSizes)
    largestBasinSizes.foldLeft(1)((x, y) => x * y)

def partTwo() : Int =
    try
        val heights = parseInput()
        findBasinProduct(heights)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1
