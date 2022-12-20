package Day8

import scala.io.Source

def parseInput(): IndexedSeq[IndexedSeq[String]] =
    val filename = "src/main/scala/Day8.txt"
    Source.fromFile(filename).getLines().map(line => line.split("\\s+").toIndexedSeq).toIndexedSeq

def countUniqueDigits(displayOutputs: IndexedSeq[IndexedSeq[String]]): Int =
    val identifiableDigitCounts = Set(2, 3, 4, 7)
    displayOutputs.map(l => l.drop(11)).flatten.count(s => identifiableDigitCounts.contains(s.length))

def partOne(): Int =
    try
        val digits = parseInput()
        countUniqueDigits(digits)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def createDisplaySet(display: String): Set[Char] =
    display.toList.toSet

def addToCounts(freqCount: Map[Char, Int], c: Char): Map[Char, Int] =
    if freqCount.contains(c)
    then freqCount + (c -> (freqCount(c) + 1))
    else freqCount + (c -> 1)

def countFreq(s: IndexedSeq[String]): Map[Char, Int] =
    val freqCount = Map.empty[Char, Int]
    s.flatten.foldLeft(freqCount)(addToCounts)

def determineOutput(inputs: IndexedSeq[String], outputs: IndexedSeq[String]): Int =
    // println("Starting to determine output")
    val inputSets = inputs.map(createDisplaySet)
    val outputSets = outputs.map(createDisplaySet)
    val inputCounts = countFreq(inputs)
    val (bActual, _) = inputCounts.filter((_, count) => count == 6).head
    val (eActual, _) = inputCounts.filter((_, count) => count == 4).head
    val (fActual, _) = inputCounts.filter((_, count) => count == 9).head
    // println("Determined first three actuals!")
    val digitCounts14 = Set(2, 4)
    val inputsWithout14 = inputs.filter(s => !digitCounts14.contains(s.length))
    // println("Determined inputs without 14!")
    val inputCountsWithout14 = countFreq(inputsWithout14)
    // println("Input counts")
    // println(inputCounts)
    // println("Input counts without 14")
    // println(inputCountsWithout14)
    // println("Got to determining aActual!")
    val (aActual, _) = inputCountsWithout14.filter((_, count) => count == 8).head
    // println("Got to determining cActual!")
    val (cActual, _) = inputCounts.filter((c, count) => count == 8 && c != aActual).head
    val digitCounts147 = Set(2, 3, 4)
    val inputsWithout147 = inputs.filter(s => !digitCounts147.contains(s.length))
    val inputCountsWithout147 = countFreq(inputsWithout147)
    // println("Got to determining dActual!")
    val (dActual, _) = inputCountsWithout147.filter((c, count) => count == 6 && c != fActual).head
    // println(dActual)
    // println(fActual)
    val gActual = "abcdefg".filter(c =>
        c != aActual &&
        c != bActual &&
        c != cActual &&
        c != dActual &&
        c != eActual &&       
        c != fActual 
    ).head
    // println("Determined all actuals")
    val wireMapping = Map(
        (aActual, 'a'), 
        (bActual, 'b'), 
        (cActual, 'c'), 
        (dActual, 'd'),
        (eActual, 'e'),
        (fActual, 'f'),
        (gActual, 'g')
    )
    // println("Wiremapping: ")
    // println(wireMapping)
    val numeralMap = Map(
        ("abcefg", 0),
        ("cf", 1),
        ("acdeg", 2),
        ("acdfg", 3),
        ("bcdf", 4),
        ("abdfg", 5),
        ("abdefg", 6),
        ("acf", 7),
        ("abcdefg", 8),
        ("abcdfg", 9)
    ).map((s, num) => (s.toList.toSet, num))
    // println("Numeral mapping: ")
    // println(numeralMap)
    // println("Output sets: ")
    // println(outputSets)
    outputSets.map(output => output.map(c => wireMapping(c))).map(output => numeralMap(output)).mkString.toInt
 
def sumOfOutputs(displayOutputs: IndexedSeq[IndexedSeq[String]]): Int =
    val inputs = displayOutputs.map(l => l.take(10))
    val outputs = displayOutputs.map(l => l.drop(11))
    inputs.zip(outputs).map(determineOutput).sum

def partTwo(): Int =
    try
        val digits = parseInput()
        sumOfOutputs(digits)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1