package Day11

import scala.io.Source
import scala.collection.immutable.Queue

def parseInput(): Vector[Vector[Int]] =
    val filename = "src/main/scala/Day11.txt"
    Source.fromFile(filename).getLines().map(line => line.toCharArray.map(c => c.asDigit).toVector).toVector

def cascadeCoord(state: (Vector[Vector[Int]], List[(Int, Int)]), coord: (Int ,Int)): (Vector[Vector[Int]], List[(Int, Int)]) =
    val (i, j) = coord
    val (levels, cascades) = state
    if i < 0 || j < 0 || i >= levels.length || j >= levels.head.length then state
    else
        val updatedLevels = levels.updated(i, levels(i).updated(j, levels(i)(j) + 1))
        if updatedLevels(i)(j) == 10 then (updatedLevels, coord :: cascades)
        else (updatedLevels, cascades)

def cascadeFlashes(energyLevels: Vector[Vector[Int]], coordsToCheck: Queue[(Int, Int)]): Vector[Vector[Int]] =
    // println(s"coords in queue ${coordsToCheck.length}")
    coordsToCheck.dequeueOption match
        case dequeuedCoord: Some[((Int, Int), Queue[(Int, Int)])] =>
            val ((i, j), restToCheck) = dequeuedCoord.value
            val adjacentCoords = List(
                (i-1,j-1),
                (i-1,j),
                (i-1,j+1),
                (i,j-1),
                (i,j+1),
                (i+1,j-1),
                (i+1,j),
                (i+1,j+1),
            )
            val state = (energyLevels, List.empty[(Int, Int)])
            val (levelsAfterFlash, cascadesNeededToCheck) = adjacentCoords.foldLeft(state)(cascadeCoord)
            // println(s"new cascades to be queued: $cascadesNeededToCheck")
            val newCoordsToCheck = restToCheck.enqueueAll(cascadesNeededToCheck)
            // println(s"coords to check in queue: ${newCoordsToCheck.length}")
            cascadeFlashes(levelsAfterFlash, newCoordsToCheck)
        case None => energyLevels

def takeStepAndCountFlashes(state: (Int, Vector[Vector[Int]])): (Int, Vector[Vector[Int]]) =
    val (flashes, energyLevels) = state
    val levelsAfterStep = takeStep(energyLevels)
    val flashesThisStep = levelsAfterStep.flatten.filter(level => level == 0).length
    (flashesThisStep + flashes, levelsAfterStep)
    

def takeStep(energyLevels: Vector[Vector[Int]]): Vector[Vector[Int]] =
    val increasedLevels = energyLevels.map(row => row.map(level => level + 1))
    val coordsThatFlashInitially = increasedLevels
        .zipWithIndex
        .map(
            (row, i) => row
                .zipWithIndex
                .map(
                    (entry, j) => (entry, (i,j)))
                    .filter((level, _) => level > 9)
                    .map((_, coord) => coord))
        .flatten
    // println(s"First initial flashes: $coordsThatFlashInitially")
    val checkedCoords = Set.empty[(Int, Int)]    
    val levelsAfterAllFlashes = cascadeFlashes(increasedLevels, Queue.from(coordsThatFlashInitially))    
    levelsAfterAllFlashes.map(row => row.map(level => if level > 9 then 0 else level))

def numberOfFlashes(energyLevels: Vector[Vector[Int]], steps: Int): Int =
    val start = (0, energyLevels)
    val (flashes, _) = (1 to steps).foldLeft(start)((energyLevelsAndFlashes, _) => takeStepAndCountFlashes(energyLevelsAndFlashes))
    flashes

def stepsUntilCoordinated(energyLevels: Vector[Vector[Int]], currentStep: Int): Int =
    if energyLevels.flatten.filter(level => level != 0).length == 0
    then currentStep
    else
        val levelsAfterStep = takeStep(energyLevels) 
        stepsUntilCoordinated(levelsAfterStep, currentStep + 1)

def partOne(): Int =
    try
        val energyLevels = parseInput()
        numberOfFlashes(energyLevels, 100)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        e.printStackTrace
        1

def partTwo() : Int =
    try
        val energyLevels = parseInput()
        stepsUntilCoordinated(energyLevels, 0)        
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        e.printStackTrace
        1
