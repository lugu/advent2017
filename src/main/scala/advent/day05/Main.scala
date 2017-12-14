package advent.day05

import scala.io.Source

object Main extends App {

  def source = Source.fromResource("day05.txt").getLines.map(_.toInt).toArray

  def move(step: Int, i: Int, maze: Array[Int], offset: Map[Int, Int]): Int = {
    if (i < 0 || i >= maze.size) step
    else
      move(step + 1,
           i + maze(i) + offset.getOrElse(i, 0),
           maze,
           offset + (i -> (offset
             .getOrElse(i, 0) + (if (maze(i) + offset.getOrElse(i, 0) >= 3) -1
                                 else 1))))
  }

  println(move(0, 0, source, Map()))
}
