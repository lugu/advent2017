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
  def dance(movements: Seq[Move]): Dancers = movements.foldLeft(this) {
    case (dancers, move) => dancers.doMove(move)
  }
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

  def nTimes(n: Int, movements: Seq[Move]): Dancers = if (n == 0) this else dance(movements).nTimes(n - 1, movements)
  def danceUntil(n: Int = 0, test: Dancers => Boolean, movements: Seq[Move]): Int = if (n != 0 && test(this)) n else dance(movements).danceUntil(n + 1, test, movements)

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

  def aq = ('a' to 'p').toSeq
  def init = Dancers(aq)
  println("one: " + init.dance(movements))
  val timesToABC = init.danceUntil(0, _.toString == aq.mkString, movements)
  println("one billion: " + init.nTimes(1000*1000*1000%timesToABC, movements))
}
