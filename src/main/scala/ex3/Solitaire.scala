package ex3

import scala.collection.IterableFactory

object Solitaire extends App:

  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  val w = 5
  val h = 5
  given IterableFactory = LazyList(_)
  
  def placeNumber(): Iterable[Solution] = 
    //TODO 
    ???

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  placeNumber().zipWithIndex.foreach({solution => println("Solution " + solution._2); println(render(solution._1.toList, w, h))})