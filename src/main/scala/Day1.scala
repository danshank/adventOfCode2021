package Day1

import java.io.BufferedInputStream
import scala.io.BufferedSource
import scala.io.Source

def partOne(): Int =
    val filename = "src/main/scala/Day1.txt"
    try
        val input = Source.fromFile(filename).getLines().map(s => s.toInt)
        countIncrements(input)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        0

def partTwo(): Int =
    val filename = "src/main/scala/Day1.txt"
    try
        val input = Source.fromFile(filename).getLines().map(s => s.toInt)
        detectAverageDepthIncrease(input)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        0

def countIncrements(depths: Iterator[Int]): Int =
    val initialCountAndPosition = (-1, 0)
    val (finalCount, _) = depths.foldLeft(initialCountAndPosition)(increaseCountIfGreater)
    finalCount

def increaseCountIfGreater(previous: (Int, Int), x: Int): (Int, Int) =
    val (n, y) = previous
    if x > y
    then (n + 1, x)
    else (n, x)

def detectAverageDepthIncrease(depths: Iterator[Int]): Int =
    val a = depths.sliding(3, 1)
    countIncrements(depths.sliding(3, 1).map(xs => xs.sum))

def safeParseToInt(s: String): Option[Int] =
    try 
        Some(s.toInt)
    catch
        case e: Exception => 
            println(s"Error parsing $s")
            println(s"Exception occurred: $e")        
            None