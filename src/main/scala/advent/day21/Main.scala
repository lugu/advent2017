package advent.day21

object Main extends App {

  trait Grid {
    def rotate90: Grid
    def rotate180: Grid = rotate90.rotate90
    def rotate270: Grid = rotate180.rotate90
    def flipH: Grid
    def flipV: Grid = flipH.rotate180
    def rotations = Seq(this, rotate90, rotate180, rotate270)
    def flips = Seq(flipH, flipV)
    def transformations: Seq[Grid] = rotations ++ flips ++ flips.flatMap(_.rotations)
    def transforms: Grid
    def transforms(n: Int): Grid = {
      println(s"step $n size $size")
      if (n == 0) this else transforms.transforms(n - 1)
    }
    def lines: Seq[String]
    def size = lines.size
    def count: Int = toString.filter(_ == '#').size
    override def toString = lines.mkString("\n")
    def at(x: Int, y: Int): String = if (x < size && y < size) 
    lines(x).toCharArray.apply(y).toString
    else  throw new Exception(s"wrong index $x, $y on $this")
    def doesMatch(o: Grid): Boolean = transformations.contains(o)
    def merge(o: Grid): Grid= GridN(lines.zip(o.lines).map{ case (a, b) => a + b })
  }

  trait GridString extends Grid {
    def transforms: Grid = rules.find(r => doesMatch(r.from)) match {
      case Some(rule) => rule.to case None => throw new Exception(s"no rules to match $this")
    }
  }

  case class Grid2(lines: Seq[String]) extends GridString {
    def rotate90 = Grid2(Seq(at(1, 0) + at(0, 0), at(1, 1) + at(0, 1)))
    def flipH = Grid2(lines.reverse)
  }
  case class Grid3(lines: Seq[String]) extends GridString {
    def rotate90 = Grid3(Seq(at(2, 0) + at(1, 0) + at(0, 0),
      at(2, 1) + at(1, 1) + at(0, 1),
      at(2, 2) + at(1, 2) + at(0, 2)))
    def flipH = Grid3(lines.reverse)
  }

  case class GridN(lines: Seq[String]) extends Grid {
    def rotate90 = ???
    def flipH = ???
    def transforms: Grid = {
      val grid: Seq[Seq[Grid]] = if (size % 2 == 0) {
        lines.map(_.grouped(2).toList).grouped(2).toList.map {
          case Seq(top, bottom) => top.zip(bottom).map {
            case (a, b) => Grid2(Seq(a,b))
          }
        }
      } else {
        lines.map(_.grouped(3).toList).grouped(3).toList.map {
          case Seq(top, middle, bottom) =>
            top.zip(middle).zip(bottom).map {
              case ((a, b), c) => Grid3(Seq(a, b, c))
            }
        }
      }
      val grid2: Seq[Seq[Grid]] = grid.map(l => l.map(_.transforms))
      val grid3: Seq[Grid] = grid2.map(l => l.reduce(_.merge(_)))
      GridN(grid3.map(_.lines).flatten)
    }
  }

  def split(s: String): Seq[String] = s.split("/").toSeq
  def init = Grid3(split(".#./..#/###"))
  def input = scala.io.Source.fromResource("day21.txt").getLines.toSeq
  // def input = List("../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#")
  
  def init2 = Grid2(split("ab/cd"))
  def input2 = List("ab/cd => abc/def/ghi", "abc/def/ghi => efij/ghkl/mnqr/opst",
  "ef/gh => 111/111/111", 
  "ij/kl => 222/222/222",
  "mn/op => 333/333/333",
  "qr/st => 444/444/444",
  "111/111/111 => efef/ghgh/efef/ghgh", 
  "222/222/222 => ijij/klkl/ijij/klkl",
  "333/333/333 => mnmn/opop/mnmn/opop",
  "444/444/444 => qrqr/stst/qrqr/stst",
  )
  def nb = 18

  trait Rule {
    def from: GridString
    def to: Grid
    override def toString = "rule\n" + from.toString + " =>\n" + to.toString
  }
  case class Rule2(from: Grid2, to: Grid3) extends Rule
  case class Rule3(from: Grid3, to: GridN) extends Rule

  def rules = input.map(r => r.split(" => ").toList match {
    case List(a, b) => if (split(a).size == 2) Rule2(Grid2(split(a)), Grid3(split(b)))
    else Rule3(Grid3(split(a)), GridN(split(b)))
  })

  // println(init.transforms(nb))
  println(init.transforms(nb).count)
}
