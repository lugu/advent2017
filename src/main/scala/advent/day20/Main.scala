package advent.day20

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Main extends App {

  case class Vector(x: Int, y: Int, z: Int) {
    import scala.math.abs
    def manhattan = abs(x) + abs(y) + abs(z)
    def +(o: Vector) = Vector(x + o.x, y + o.y, z + o.z)
    def *(s: Int) = Vector(s * x, s * y, s * z)
  }

  case class Particule(p: Vector, v: Vector, a: Vector) {
    def next: Particule = Particule(p + v + a, v + a, a)
  }

  class ParticulParser extends RegexParsers {
    // p=<-1027,-979,-188>, v=<7,60,66>, a=<9,1,-7>
    def num: Parser[Int] = "(-?\\d+)".r ^^ { case p => p.toInt }
    def numSeq: Parser[Seq[Int]] = rep1(num, "," ~> num)
    def vector: Parser[Vector] =
      "[a-z]=<".r ~> numSeq <~ ">" ^^ (l =>
        if (l.size != 3) throw new Exception("vector")
        else Vector(l(0), l(1), l(2)))
    def particule: Parser[Particule] =
      rep1(vector, ", " ~> vector) ^^ (l =>
        if (l.size != 3) throw new Exception("particule")
        else Particule(l(0), l(1), l(2)))

    def analyse(input: String): Particule = {
      parse(particule, input) match {
        case Success(matched, next) => {
          if (!next.atEnd) throw new Exception("Parsed up to: " + next.offset)
          matched
        }
        case Failure(msg, next) => throw new Exception("Failure: " + msg)
        case Error(msg, next) => throw new Exception("Error: " + msg)
      }
    }
  }

  def input = Source.fromResource("day20.txt").getLines.toSeq
  def particules: Seq[Particule] =
    input.map((new ParticulParser).analyse(_)).toSeq
  def removeCollision(p: Seq[Particule]): Seq[Particule] = {
    val posToRemove: Seq[Vector] =
      p.groupBy(_.p).filter(_._2.size > 1).keys.toSeq
    p.filter(a => !posToRemove.contains(a.p))
  }
  def loop(n: Int, p: Seq[Particule]): Seq[Particule] = {
    if (n == 0) p
    else loop(n - 1, removeCollision(p.map(_.next)))
  }
  println(loop(10000, particules).toList.size)
}
