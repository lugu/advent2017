package advent.day19

import scala.io.Source

object Main extends App {

  case class InvalidDirection(p: Packet) extends Exception {
    override def toString = s"Invalid direction $p"
  }

  trait Direction extends Point {
    def turn: Tuple2[Direction, Direction]
  }
  object Left extends Point(-1, 0) with Direction {
    def turn = (Up, Down)
  }
  object Right extends Point(1, 0) with Direction {
    def turn = (Up, Down)
  }
  object Up extends Point(0, -1) with Direction {
    def turn = (Right, Left)
  }
  object Down extends Point(0, 1) with Direction {
    def turn = (Right, Left)
  }

  case class Point(x: Int, y: Int) {
    def add(o: Point) = Point(x + o.x, y + o.y)
    def follow(d: Direction) = add(d)
  }

  case class Packet(pos: Point,
                    dir: Direction,
                    string: String,
                    grid: Map[Point, Char],
                    dist: Int = 0) {

    override def toString = s"(pos $pos), (dir $dir), ($string) (dist: $dist)"
    def canGo(p: Point, d: Direction): Boolean =
      try {
        Packet(p, d, string, grid).next
        true
      } catch {
        case InvalidDirection(a) => false
      }
    def nextPos = pos.follow(dir)
    def nextLoop: Packet = next.nextLoop
    def next: Packet = {
      grid.get(pos.follow(dir)) match {
        case Some('+') => {
          val (a, b) = dir.turn
          val newDir = if (canGo(nextPos, a)) a else b
          Packet(nextPos, newDir, string, grid, dist + 1)
        }
        case Some('-') | Some('|') =>
          Packet(pos.follow(dir), dir, string, grid, dist + 1)
        case Some(a) =>
          Packet(pos.follow(dir), dir, string + a, grid, dist + 1)
        case None => throw new InvalidDirection(this)
      }
    }
  }

  def input = Source.fromResource("day19.txt").getLines.toSeq

  def grid: Map[Point, Char] =
    input.zipWithIndex.flatMap {
      case (s, y) => {
        s.zipWithIndex.filter { case (c, p) => c != ' ' }.map {
          case (c, x) => (Point(x, y), c)
        }
      }
    }.toMap

  val init = Packet(Point(77, 0), Down, "", grid)

  try {
    init.nextLoop
  } catch {
    case InvalidDirection(p) => println(p)
  }
}
