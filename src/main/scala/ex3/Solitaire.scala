package ex3

import scala.collection.IterableFactory

// object Solitaire extends App:

//   type Position = (Int, Int)
//   type Solution = Iterable[Position]
//   type IterableFactory = Solution => Iterable[Solution]
//   val width = 4
//   val height = 4
//   given IterableFactory = LazyList(_)

//   def inside(position: (Int, Int)): Boolean =
//     val (x, y) = position
//     x >= 0 && x < width && y >= 0 && y < height

//   def isSafe(position: Position, solution: Iterable[Position]): Boolean =
//     !solution.exists(p => p == position) && inside(position)

//   def placeMarks(): Iterable[Solution] =
//     def _placeMarks(grid: List[Position] = List())(using factory: IterableFactory): Iterable[Solution] = grid.length match
//       case n if n == width*height => factory(grid)
//       case _ =>
//         for
//           move <- List((-3, 0), (3, 0), (0, -3), (0, 3), (2, 2), (-2, -2), (2, -2), (-2, 2))
//           lastMarkPosition = grid.head
//           newMarkPosition = (lastMarkPosition._1 + move._1, lastMarkPosition._2 + move._2)
//           if isSafe(newMarkPosition, grid)
//           solution <- _placeMarks(newMarkPosition :: grid)
//         yield
//           solution
//     _placeMarks(List((width / 2, height / 2)))

//   def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
//     val reversed = solution.reverse
//     val rows =
//       for y <- 0 until height
//         row = for x <- 0 until width
//         number = reversed.indexOf((x, y)) + 1
//         yield if number > 0 then "%-2d ".format(number) else "X  "
//       yield row.mkString
//     rows.mkString("\n")

//   placeMarks().zipWithIndex.foreach({solution => println(); println(s"sol ${solution._2}"); println(render(solution._1.toSeq,width,height))})
 


object Solitaire extends App:

  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  val w = 7
  val h = 5
  given IterableFactory = LazyList(_)
  
  private def isValid(curr: Cell, next: Cell): Boolean =
    val x: Int = Math.abs(curr._1 - next._1)
    val y: Int = Math.abs(curr._2 - next._2)
    (x == 3 && y == 0) || (x == 0 && y == 3) || (x == 2 && y == 2)

  private def isSafe(cell: Cell, solution: Iterable[Cell]): Boolean =
    !solution.exists(p => p == cell || isValid(p, cell))

  private def placeMarks(n: Int = w*h)(using factory: IterableFactory): Iterable[Solution] = n match
    case 1 => factory(List((w/2, h/2)))
    case _ => 
      val solutions = placeMarks(n-1)
      for
        sol <- solutions
        x <- 0 until w
        y <- 0 until h
        if isSafe((x,y), sol)
      yield sol.++(List((x,y)))
    //TODO 
    

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  placeMarks().zipWithIndex.foreach({solution => println("Solution " + solution._2); println(render(solution._1.toList, w, h))})