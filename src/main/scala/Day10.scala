package Day10

import scala.io.Source
import scala.collection.mutable.Stack

def parseInput(): Vector[Vector[Char]] =
    val filename = "src/main/scala/Day10.txt"
    Source.fromFile(filename).getLines().map(line => line.toCharArray.toVector).toVector

def scoreLine(line: Vector[Char], symbolMap: Map[Char, Char], scores: Map[Char, Int], parsed: Stack[Char]): Int =
    line.headOption match
        case d: Some[Char] =>
            val c = d.value
            // println(s"Checking char $c against stack $parsed")
            if symbolMap.contains(c)
            then
                if parsed.pop() != symbolMap(c) then scores(c)
                else
                    scoreLine(line.tail, symbolMap, scores, parsed)
            else
                parsed.push(c)
                scoreLine(line.tail, symbolMap, scores, parsed)
        case None => 0
            

def scoreOfFirstErrors(chunks: Vector[Vector[Char]]): Int =
    val symbolMap = Map((')', '('), (']', '['), ('}', '{'), ('>', '<'))
    val scores = Map((')', 3), (']', 57), ('}', 1197), ('>', 25137))
    chunks.map(line => scoreLine(line, symbolMap, scores, Stack.empty[Char])).sum

def partOne(): Int =
    try
        val chunks = parseInput()        
        scoreOfFirstErrors(chunks)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def calcScoreCompletion(scores: Seq[Int]): BigInt =
    println(scores)
    var score: BigInt = 0
    for (x <- scores)
        score = score * 5 + x
    score

def scoreLineCompletion(line: Vector[Char], symbolMap: Map[Char, Char], scores: Map[Char, Int], parsed: Stack[Char]): BigInt =
    line.headOption match
        case c: Some[Char] =>
            if symbolMap.contains(c.value)
            then
                if parsed.pop() != symbolMap(c.value) then 0
                else
                    scoreLineCompletion(line.tail, symbolMap, scores, parsed)
            else
                parsed.push(c.value)
                scoreLineCompletion(line.tail, symbolMap, scores, parsed)
        case None =>        
            calcScoreCompletion(parsed.popAll.map(c => scores(c)).toSeq.reverse)

            

def scoreOfLineCompletions(chunks: Vector[Vector[Char]]): BigInt =
    val symbolMap = Map((')', '('), (']', '['), ('}', '{'), ('>', '<'))
    val scoreMap = Map(('(', 1), ('[', 2), ('{', 3), ('<', 4))
    val scores = chunks.map(line => scoreLineCompletion(line, symbolMap, scoreMap, Stack.empty[Char])).filter(score => score != 0).sorted
    println(scores)
    println(scores.length)
    scores(scores.length / 2)


def partTwo() : BigInt =
    try
        val chunks = parseInput()
        scoreOfLineCompletions(chunks)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        e.printStackTrace
        1
