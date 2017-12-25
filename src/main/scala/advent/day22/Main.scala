package advent.day22

object Main extends App {

  case class Point(x: Int, y: Int) {
    def add(o: Point) = Point(x + o.x, y + o.y)
    def turnLeft: Point = Point(y, -1 * x)
    def turnRight: Point = Point(-1 * y, x)
    def turnOpposite: Point = Point(-1 * x, -1 * y)
  }

  val north = Point(0, -1)

  object State {
    val infected = '#'
    val clean = '.'
    val flag = 'F'
    val weak = 'W'
  }

  case class Virus(pos: Point, dir: Point) {
    def left = Virus(pos, dir.turnLeft)
    def right = Virus(pos, dir.turnRight) 
    def forward = Virus(pos.add(dir), dir)
    def move(map: Map[Point,Char]): Tuple2[Virus,Map[Point,Char]] = {
      val newDir = state(map, pos) match {
        case State.clean => dir.turnLeft
        case State.infected => dir.turnRight
        case State.flag => dir.turnOpposite
        case State.weak => dir
      }
      val v2 = Virus(pos, newDir)
      (v2.forward, switch(map, pos))
    }
  }

  def switch(map: Map[Point,Char], pos: Point): Map[Point,Char] = {
    val newState = state(map, pos) match {
        case State.clean => State.weak
        case State.infected => State.flag
        case State.flag => State.clean
        case State.weak => State.infected
    }
    map + (pos -> newState)
  }
  def state(map: Map[Point,Char], pos: Point): Char = map.getOrElse(pos, State.clean)
  def isInfected(map: Map[Point,Char], pos: Point): Boolean = state(map, pos) match {
    case State.infected => true
    case _ => false
  }

  def input = scala.io.Source.fromResource("day22.txt").getLines.toSeq
  def center = Point(input.size/2, input.size/2)
  def virus = Virus(center, north)

  def grid: Map[Point, Char] =
    input.zipWithIndex.flatMap {
      case (s, y) => {
        s.zipWithIndex.filter { case (c, p) => c != State.clean }.map {
          case (c, x) => (Point(x, y), c)
        }
      }
    }.toMap
  def move(n: Int, v: Virus, m: Map[Point,Char]) {
    if (n == 0) return
    val (v2, m2) = v.move(m)
    move(n - 1, v2, m2)
  }
  move(10 * 1000 * 1000, virus, grid)
}
