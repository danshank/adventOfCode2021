package Day12

import scala.io.Source
import scala.collection.mutable.Stack

enum Node:
    case Start, End 
    case SmallCave(id: String)
    case LargeCave(id: String)

def createNode(s: String): Node =
    if s == "start" then Node.Start
    else if s == "end" then Node.End
    else if s == s.toUpperCase then Node.LargeCave(s)
    else Node.SmallCave(s)

def addNodesToMap(a: Node, b: Node, map: Map[Node, Set[Node]]): Map[Node, Set[Node]] =
    (a, b) match
        case (Node.End, _) => map
        case (_, Node.Start) => map
        case _ =>
            if map.contains(a)
            then
                val withB = map(a) + b
                map + (a -> withB)
            else
                map + (a -> Set(b))

def parseLine(existingMap: Map[Node, Set[Node]], line: String): Map[Node, Set[Node]] =
    val connection = "([a-zA-Z]+)-([a-zA-Z]+)".r
    line match
        case connection(a, b) =>
            val (an, bn) = (createNode(a), createNode(b))
            val withA = addNodesToMap(an, bn, existingMap)
            addNodesToMap(bn, an, withA)

def parseInput(): Map[Node, Set[Node]] =
    val filename = "src/main/scala/Day12.txt"
    val caves = Map.empty[Node, Set[Node]]
    Source.fromFile(filename).getLines().foldLeft(caves)(parseLine)

def visitNode(caveMap: Map[Node, Set[Node]], path: Stack[Node], nodesNotToVisit: Set[Node], nodeToVisit: Node, revisited: Boolean): Int =
    val revisitingSmallCave = nodesNotToVisit.contains(nodeToVisit)
    // if revisitingSmallCave && revisited then
    //     0
    // else
    val newPath = path.push(nodeToVisit)
    searchForPaths(caveMap, path, nodesNotToVisit, revisitingSmallCave)

def getAndVisitNodes(caveMap: Map[Node, Set[Node]], path: Stack[Node], nodesNotToVisit: Set[Node], revisited: Boolean): Int =
    val node = path.top
    val nodesToVisit = if revisited then caveMap(node).diff(nodesNotToVisit) else caveMap(node)
    if nodesToVisit.size == 0 then
        0
    else 
        nodesToVisit.foldLeft(0)((pathsToEnd, nodeToVisit) => visitNode(caveMap, path, nodesNotToVisit, nodeToVisit, revisited) + pathsToEnd)

def searchForPaths(caveMap: Map[Node, Set[Node]], path: Stack[Node], nodesNotToVisit: Set[Node], revisited: Boolean): Int =
    val pathsToFinish = path.top match
        case Node.End =>
            // println(path)
            1
        case node: Node.SmallCave =>
            val withoutCave = nodesNotToVisit + node
            getAndVisitNodes(caveMap, path, withoutCave, revisited)
        case node: _ =>
            getAndVisitNodes(caveMap, path, nodesNotToVisit, revisited)
    path.pop()
    pathsToFinish

def countPaths(caveMap: Map[Node, Set[Node]]): Int =
    val nodesNotToVisit = Set.empty[Node]    
    val path = Stack(Node.Start)
    searchForPaths(caveMap, path, nodesNotToVisit, true)

def partOne(): Int =
    try
        val caveMap = parseInput()
        countPaths(caveMap)
    catch case e: Exception =>
        println(s"Exception occurred: $e")
        e.printStackTrace
        1

def countPathsWithOneRevisit(caveMap: Map[Node, Set[Node]]): Int =
    val nodesNotToVisit = Set.empty[Node]
    val nodesExplored = Set.empty[Node]
    val path = Stack(Node.Start)
    searchForPaths(caveMap, path, nodesNotToVisit, false)

def partTwo(): Int =
    try
        val caveMap = parseInput()
        // println("Counting paths with one revisit")
        countPathsWithOneRevisit(caveMap)
    catch case e: Exception =>
        // println(s"Exception occurred: $e")
        // e.printStackTrace
        1
