package Day6

import scala.io.Source

def parseInput(): Array[Int] =
    val filename = "src/main/scala/Day6.txt"
    Source.fromFile(filename).getLines().next.split(",").map(s => s.toInt)
    
def numberOfOffspring(knownOffspring: Map[Int, Long], age: Int): Long =
    val daysToMaturity = 2
    val daysToBreed = 7
    knownOffspring.get(age) match
        case offspring: Some[Long] =>
            offspring.value
        case None =>
            if age < daysToBreed
            then 1
            else
                val offspringAges = (age - daysToBreed).until(-1, -daysToBreed).map(offspringAge => offspringAge - daysToMaturity)
                if (age == 6) println(offspringAges)
                val offspringCreated = offspringAges.map(offspringAge => if knownOffspring.contains(offspringAge) then knownOffspring(offspringAge) else 1)
                offspringCreated.sum + 1

def updateKnownOffspring(knownOffspring: Map[Int, Long], age: Int): Map[Int, Long] =
    knownOffspring.get(age) match
        case offspring: Some[Long] => knownOffspring
        case None => knownOffspring + (age -> numberOfOffspring(knownOffspring, age))

def getTotalOffspring(daysToBreed: Array[Int], totalDays: Int): Long =    
    val agesToCheck = 1.to(totalDays + 7)
    val knownOffspringMap = Map.empty[Int, Long]
    val updatedKnownOffspringMap = agesToCheck.foldLeft(knownOffspringMap)(updateKnownOffspring)
    // println(s"Finished creating map $updatedKnownOffspringMap")
    val ages = daysToBreed.map(d => totalDays - d + 6).toList
    // println(s"Ages: $ages")
    ages.map(age => numberOfOffspring(updatedKnownOffspringMap, age)).sum

def partOne(): Long =
    try
        val ages = parseInput()
        getTotalOffspring(ages, 80)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1

def partTwo(): Long =
    try
        val ages = parseInput()
        getTotalOffspring(ages, 256)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        1
