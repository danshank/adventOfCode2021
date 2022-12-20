package Day3

import scala.io.Source

def partOne(): Int =
    val filename = "src/main/scala/Day3.txt"
    try
        val rawInput = Source.fromFile(filename).getLines()
        val lines = rawInput.map(parseLine)
        findGammaEpsilonProduct(lines)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def parseLine(s: String): Int =
    Integer.parseInt(s, 2)

def getNthBit(x: Int, n: Int): Int =
    (x >> n) & 0x1

def bitCountAndPositionToInt(bitShiftMapping: (Int, (Int, Int))): Int =
    val (n, (zeroes, ones)) = bitShiftMapping
    if ones > zeroes
    then 0x1 << n
    else 0

def findGammaEpsilonProduct(a: Iterator[Int]): Int =
    val bitShifts = Vector.range(0, 12)
    val counts = bitShifts map (shift => (shift, (0,0))) toMap
    val finalCounts = a.foldLeft(counts)(updateCountsOfEachBit)
    val gamma = finalCounts map bitCountAndPositionToInt sum
    val epsilon = ~gamma & 0xFFF
    gamma * epsilon

def updateCountsOfEachBit(counts: Map[Int, (Int, Int)], x: Int): Map[Int, (Int, Int)] =
    val updateBitShiftUsingX = (mapping) => updateBitShiftMapping(mapping, x)
    counts map updateBitShiftUsingX toMap

def updateBitShiftMapping(bitShiftMapping: (Int, (Int, Int)), x: Int): (Int, (Int, Int)) =
    val (n, (zeroes, ones)) = bitShiftMapping
    val xs = x.toBinaryString
    val nthBit = getNthBit(x, n)
    nthBit match
        case 0 => (n, (zeroes + 1, ones))
        case 1 => (n, (zeroes, ones + 1))

def partTwo(): Int =
    val filename = "src/main/scala/Day3.txt"
    try
        val rawInput = Source.fromFile(filename).getLines()
        val lines = rawInput.map(parseLine)
        findLifeSupportRating(lines.toVector)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def findLifeSupportRating(a: Vector[Int]): Int =
    val oxygen = a
    val co2 = a
    val bitShifts = Vector.range(0, 12).reverse
    val oxygenRating = bitShifts.foldLeft(oxygen)(findOxygenRating)
    val co2Rating = bitShifts.foldLeft(co2)(findCO2Rating)
    oxygenRating.head * co2Rating.head

def findOxygenRating(oxygen: Vector[Int], bitShift: Int): Vector[Int] =
    val candidates = oxygen.length
    val startingCount = (bitShift,(0, 0))
    val (_, (zeroes, ones)) = oxygen.foldLeft(startingCount)(updateBitShiftMapping)
    if zeroes > ones
    then oxygen.filter(x => getNthBit(x, bitShift) == 0)
    else oxygen.filter(x => getNthBit(x, bitShift) == 1)

def findCO2Rating(co2: Vector[Int], bitShift: Int): Vector[Int] =
    if co2.length == 1
    then co2
    else
        val candidates = co2.length
        val startingCount = (bitShift,(0, 0))
        val (_, (zeroes, ones)) = co2.foldLeft(startingCount)(updateBitShiftMapping)
        if zeroes > ones
        then co2.filter(x => getNthBit(x, bitShift) == 1)
        else co2.filter(x => getNthBit(x, bitShift) == 0)
