package advent.day16
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens

trait Move
case class Spin(n: Int) extends Move { def chars = toString }
case class Exchange(x: Int, y: Int) extends Move { def chars = toString }
case class Partner(a: Char, b: Char) extends Move { def chars = toString }

case class Dancers(dancers: Seq[Char]) {
  override def toString = dancers.mkString
  def doMove(move: Move): Dancers = move match {
    case Spin(n) => {
      if (n < 0 || n >= dancers.size)
        throw new Exception("invalid spin size " + n)
      Dancers(dancers.takeRight(n) ++ dancers.dropRight(n))
    }
    case Exchange(x, y) => {
      val a = dancers(x)
      val b = dancers(y)
      val mutable = dancers.toArray
      mutable(x) = b
      mutable(y) = a
      Dancers(mutable.toSeq)
    }
    case Partner(a, b) => {
      val x = dancers.indexOf(a)
      val y = dancers.indexOf(b)
      val mutable = dancers.toArray
      mutable(x) = b
      mutable(y) = a
      Dancers(mutable.toSeq)
    }
  }
}

class DanceCollector extends RegexParsers {
  override def skipWhitespace = false
  def coma: Parser[String] = ","
  def position: Parser[Int] = raw"(\d+)".r ^^ { case p => p.toInt }
  def name: Parser[Char] = raw"(\w)".r ^^ { case n => n.toCharArray.head }
  def spin: Parser[Spin] = "s" ~> position ^^ (Spin(_))
  def exchange: Parser[Exchange] = "x" ~> position ~ "/" ~ position ^^ {
    case (a ~ b ~ c) => Exchange(a, c)
  }
  def partner: Parser[Partner] = "p" ~> name ~ "/" ~ name ^^ {
    case (a ~ b ~ c) => Partner(a, c)
  }
  def movement = spin | exchange | partner
  def dance = rep1(movement, coma ~> movement)

  def analyse(input: String): Seq[Move] = {
    parse(dance, input) match {
      case Success(matched, next) => {
        if (!next.atEnd) throw new Exception("Parsed up to: " + next.offset)
        matched
      }
      case Failure(msg, next) => throw new Exception("Failure: " + msg)
      case Error(msg, next) => throw new Exception("Error: " + msg)
    }
  }
}

object Main extends App {
  def input = Source.fromResource("day16.txt").getLines.toSeq.head
  def parser = (new DanceCollector)
  def movements: Seq[Move] = parser.analyse(input)

  def init = Dancers(('a' to 'p').toSeq)
  def result = movements.foldLeft(init) {
    case (dancers, move) => dancers.doMove(move)
  }
  println(result)
}
