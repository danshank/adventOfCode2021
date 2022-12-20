package Day5

import scala.io.Source
import scala.compiletime.ops.string
import scala.util.matching.Regex

case class Coord(
    x: Int,
    y: Int
)

def parseLine(coordinate: Regex, line: String): (Coord, Coord) =
    line match
        case coordinate(x1, y1, x2, y2) =>
            (Coord(x1.toString.toInt, y1.toString.toInt), Coord(x2.toString.toInt, y2.toString.toInt))

def parseCoords(): Iterator[(Coord, Coord)] =
    val filename = "src/main/scala/Day5.txt"
    val rawInput = Source.fromFile(filename).getLines()
    val coordinate = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
    rawInput.map(l => parseLine(coordinate, l))

def mapCoord(coordsMap: Map[Coord, Int], coord: Coord): Map[Coord, Int] =
    if coordsMap.contains(coord)
    then coordsMap + (coord -> (coordsMap(coord) + 1))
    else coordsMap + (coord -> 1)

def getCoordsBetweenEndpoints(coordEndpoints: (Coord, Coord)): IndexedSeq[Coord] =
    val (a, b) = coordEndpoints
    if a.x == b.x
    then 
        val range = if (a.y < b.y) then (a.y to b.y) else (b.y to a.y)
        range.map(y => Coord(a.x, y))
    else 
        if a.y == b.y
        then 
            val range = if (a.x < b.x) then (a.x to b.x) else (b.x to a.x)
            range.map(x => Coord(x, a.y))
        else
            (a.x < b.x, a.y < b.y) match
                case (true, true) => (a.x to b.x).zip(a.y to b.y).map((x, y) => Coord(x, y))
                case (true, false) => (a.x to b.x).zip(a.y.to(b.y, -1)).map((x, y) => Coord(x, y))
                case (false, true) => (a.x.to(b.x, -1)).zip(a.y to b.y).map((x, y) => Coord(x, y))
                case (false, false) => (a.x.to(b.x, -1)).zip(a.y.to(b.y, -1)).map((x, y) => Coord(x, y))


def mapCoordLine(coordsMap: Map[Coord, Int], coordEndpoints: (Coord, Coord)): Map[Coord, Int] =
    val (a, b) = coordEndpoints
    if a.x == b.x
    then 
        val range = if (a.y < b.y) then (a.y to b.y) else (b.y to a.y)
        range.map(y => Coord(a.x, y)).foldLeft(coordsMap)(mapCoord)
    else 
        if a.y == b.y
        then 
            val range = if (a.x < b.x) then (a.x to b.x) else (b.x to a.x)
            range.map(x => Coord(x, a.y)).foldLeft(coordsMap)(mapCoord)
        else coordsMap

def mapCoordLineWithDiagonals(coordsMap: Map[Coord, Int], coordEndpoints: (Coord, Coord)): Map[Coord, Int] =
    val coords = getCoordsBetweenEndpoints(coordEndpoints)
    if (coords.size == 0) then println(coordEndpoints)
    coords.foldLeft(coordsMap)(mapCoord)


def pointsThatOverlap(lines: Iterator[(Coord, Coord)]): Int =
    val coordMap = Map.empty[Coord, Int]
    lines.foldLeft(coordMap)(mapCoordLine).count((_, timesShown) => timesShown > 1)

def pointsThatOverlapWithDiagonals(lines: Iterator[(Coord, Coord)]): Int =
    val coordMap = Map.empty[Coord, Int]
    lines.foldLeft(coordMap)(mapCoordLineWithDiagonals).count((_, timesShown) => timesShown > 1)

def partOne(): Int =
    try
        val coords = parseCoords()
        pointsThatOverlap(coords)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def partTwo(): Int =
    try
        val coords = parseCoords()
        pointsThatOverlapWithDiagonals(coords)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1
