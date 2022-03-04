package agh.tw.lab5

import scala.io.Source.*
import scala.util.Random

object TraceTool {
  def areDependent(instruction: String, otherInstruction: String): Boolean =
    val (lhs, rhs): (String, String) = splitInstruction(instruction)
    val (otherLhs, otherRhs): (String, String) = splitInstruction((otherInstruction))

    val (label, otherLabel): (Char, Char) = (lhs.charAt(0), otherLhs.charAt(0))
    val (lValue, otherlValue): (String, String) = (getLvalue(lhs), getLvalue(otherLhs))

    (otherRhs.contains(lValue) || rhs.contains(otherlValue))

  def splitInstruction(instruction: String): (String, String) =
    instruction.split('=') match {
      case Array(lhs, rhs) => (lhs, rhs)
    }

  def getLvalue(lhs: String): String =
    lhs.split(" ")(1)

  def findDependencies(instructions: Seq[String]): (Seq[(Char, Char)], Seq[(Char, Char)]) =
    val allPairs: Seq[(String, String)] = for {
      instruction <- instructions
      otherInstruction <- instructions
    } yield (instruction, otherInstruction)

    val (dependentInstructions, independentInstructions): (Seq[(String, String)], Seq[(String, String)]) = {
      allPairs.partition((instr, otherInstr) => areDependent(instr, otherInstr))
    }
    (extractLabels(dependentInstructions), extractLabels(independentInstructions))


  def extractLabels(instructionPairs: Seq[(String, String)]): Seq[(Char, Char)] =
    instructionPairs.map{
      (instruction, otherInstruction) => (instruction.charAt(0), otherInstruction.charAt(0))
    }


// algorithm from Diekert's paper (not used)
  def findHasse(i: Int, minSet: Set[Int], edges: Set[(Int, Int)], levels: Vector[Int],
                dInstructions: Set[(Char, Char)], trace: String):
  (Set[(Int, Int)], Vector[Int]) = {
    if (i >= 0){
      val newEdges: Set[(Int, Int)] = for {
        vertex <- minSet
        vertexChar = trace.charAt(vertex)
        currentChar = trace.charAt(i)
        if dInstructions.contains((currentChar, vertexChar))
      } yield ((i, vertex))

      val newMinSet: Set[Int] = for {
        vertex <- minSet
        vertexChar = trace.charAt(vertex)
        currentChar = trace.charAt(i)
        if !dInstructions.contains((currentChar, vertexChar))
      } yield vertex

      val newEdge: Option[(Int, Int)] = newEdges.iterator.nextOption()
      val newLevels: Vector[Int] = newEdge match {
        case edge: Some[(Int, Int)] => {
          val succ: Int = edge.get(1)
          levels.updated(i, levels(succ) + 1)
        }
        case None => levels
      }
      findHasse(i-1, newMinSet + i, edges ++ newEdges, newLevels, dInstructions, trace)
    }
    else (edges, levels)
  }

  def fullGraph(idx: Int, edges: Set[(Int, Int)],
                    trace: String, dInstructions: Set[(Char, Char)]): Set[(Int, Int)] = {
    if(idx < trace.length){
      val newEdges: Seq[(Int, Int)] = for {
         otherIdx <- idx + 1 until trace.length
         letter = trace.charAt(idx)
         otherLetter = trace.charAt(otherIdx)
         if (dInstructions.contains((letter, otherLetter)) ||
          letter == otherLetter)
      } yield((idx, otherIdx))
      fullGraph(idx + 1, edges ++ newEdges, trace, dInstructions)
    }
    else edges
  }

  def reducedGraph(idx: Int, remainingNeighbors:Seq[Int], edges: Set[(Int, Int)],
                   trace: String): Set[(Int, Int)] = {
    if(idx < trace.length){
      remainingNeighbors match {
        case v::vs => {
          val neighbors: Seq[Int] = getNeighbors(idx, edges)
          val reachableFromNeighbor: Set[Int] =
            getReachable(List(v), Set(), edges) - v

          val newEdges: Set[(Int, Int)] = for{
            edge <- edges
            source = edge(0)
            destination = edge(1)
            if !(source == idx && reachableFromNeighbor.contains(destination))
          } yield edge

          reducedGraph(idx, vs, newEdges, trace)
        }
        // processed all neighbors
        case Nil =>
          val newVertex = idx + 1
          reducedGraph(newVertex, getNeighbors(newVertex, edges), edges, trace)
      }
    }
      //processed whole trace
    else edges
  }

  def getNeighbors(vertex: Int, edges: Set[(Int, Int)]): Seq[Int] = {
    edges.filter((src, dest) => src == vertex)
      .map((src, dest) => dest)
      .toList
  }

  // DFS
  def getReachable(stack: List[Int], visited: Set[Int],
                   edges: Set[(Int, Int)]): Set[Int] =
    stack match {
      case v::vs =>
        val unvisitedNeighbors = getNeighbors(v, edges).filterNot(u => visited.contains(u))
        getReachable(unvisitedNeighbors ++: vs, visited + v, edges)
      case Nil => visited
    }

  def getRoots(edges: Set[(Int, Int)]): Set[Int] = {
    val (sources, destinations): (Set[Int], Set[Int]) = edges.unzip
    sources -- destinations
  }

  // function to obtain fnf from graph in top-down manner
  def foataNormalForm(factors: List[Set[Int]], edges: Set[(Int, Int)]): List[Set[Int]] = {
    val roots: Set[Int] = getRoots(edges)
    val newFactors: List[Set[Int]] = {
      factors :+ roots
    }
    val newEdges: Set[(Int, Int)] = for {
      edge <- edges
      source = edge._1
      if !roots.contains(source)
    } yield edge
    if (newEdges.size == 0){ //processed whole graph
      val sources: Set[Int] = edges.map((src, dest) => dest)
      newFactors :+ sources
    }
    else {
      foataNormalForm(newFactors, newEdges)
    }
  }

  def printResult(dependentInstructions: Seq[(Char, Char)],
                  independentInstructions: Seq[(Char, Char)],
                  reducedGraphEdges: Set[(Int, Int)],
                  factors: List[Set[Int]],
                  trace: String): Unit = {
    println("D = ")
    dependentInstructions.foreach(pair => print(pair))
    println("\nI = ")
    independentInstructions.foreach(pair => print(pair))

    println("\n\nFNF = ")
    for (factor <- factors) {
    print("(")
    factor.foreach(label => print(trace(label)))
      print(")")
    }

    println("\n\nGraph:")
    println("Edges:")
    reducedGraphEdges.foreach(edge => println(edge))
    println("Labels:")
    for {
      i <- 0 until trace.length
      letter = trace(i)
    } println(s"$i[label=$letter]")
  }

  def main(args: Array[String]): Unit = {
    if(args.length == 0) println("Pass input file.")
    else{
      val filename = args(0)
      val lines: Seq[String] = fromFile(filename).getLines.filter(_ != " ").toSeq
      val (instructions, noninstructions) = lines.partition(_.contains("="))

      val alphabet: Set[Char] = noninstructions(0).filter(_ != ' ').toSet
      val trace: String = noninstructions(1)
      instructions.filter(instr => alphabet.contains(instr(0)))

      val (dependentInstructions, independentInstructions): (Seq[(Char, Char)], Seq[(Char, Char)]) = {
        findDependencies(instructions)
      }

      val fullGraphEdges: Set[(Int, Int)] =
        fullGraph(0, Set(), trace, dependentInstructions.toSet)
      val reducedGraphEdges: Set[(Int, Int)] =
        reducedGraph(0, getNeighbors(0, fullGraphEdges), fullGraphEdges, trace)

      val factors: List[Set[Int]] = foataNormalForm(List(), reducedGraphEdges)

      printResult(dependentInstructions,
        independentInstructions,
        reducedGraphEdges,
        factors,
        trace)
    }
  }

}
