package ex3

import scala.collection.IterableFactory

object Solitaire:

  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  val w = 2
  val h = 2
  given IterableFactory = LazyList(_)
  var possibleMove = List((2, 2), (-2, -2), (2, -2), (-2, 2), (-3, 0), (3, 0), (0, -3), (0, 3))

  def isValid(curr: Cell): Boolean =
    curr._1 >= 0 && curr._1 < w && curr._2 >= 0 && curr._2 < h

  def isSafe(cell: Cell, solution: Iterable[Cell]): Boolean =
    !solution.exists(p => p == cell || isValid(cell))

  def placeMarks(n: Int = w*h, curr: Cell = (w/2, h/2), solution: Solution = List((w/2, h/2)))(using factory: IterableFactory): Iterable[Solution] = n match
    case 1 => factory(solution)
    case _ => 
      for
        move <- possibleMove
        nextCell = (curr._1 + move._1, curr._2 + move._2)
        if isSafe(nextCell, solution)
        newSolution = solution ++ List(nextCell)
        generatedSolution <- placeMarks(n-1, nextCell, newSolution)
        if generatedSolution.size == w * h
      yield generatedSolution

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

@main def TestSolitaire(): Unit =
  import Solitaire.*
  import Solitaire.given_IterableFactory
  placeMarks().zipWithIndex.foreach({solution => println("Solution " + solution._2); println(render(solution._1.toList, w, h))})