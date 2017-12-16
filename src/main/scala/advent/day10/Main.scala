package advent.day10
import scala.io.Source

object Main extends App {

  def footer = Seq(17, 31, 73, 47, 23)
  def string = (0 to 255).toSeq

  // def input = ""
  // def input = "AoC 2017"
  // def input = "1,2,3"
  // def input = "1,2,4"
  def input = Source.fromResource("day10.txt").getLines.toSeq.head

  def source: Seq[Int] = proccessInput(input)

  // part 1:
  println(initGame.play(fromAscii(input)).circle.answer)

  def pad(s: Seq[Int]) = s ++ List(17, 31, 73, 47, 23)
  def repeat(n: Int, s: Seq[Int]): List[Int] = (1 to n).toList flatMap { i =>
    s
  }
  def stringToSeq(s: String): Seq[Int] = s.toArray.map(_.toByte.toInt)
  def proccessInput(s: String) = repeat(64, pad(stringToSeq(s)))

  def fromAscii(s: String): Seq[Int] =
    s.split(",").filter(!_.isEmpty).map(_.toInt)

  case class Circle(string: Seq[Int], offset: Int) {
    def rotate(m: Int) = {
      val n = m % string.size
      Circle(string.drop(n) ++ string.take(n), (offset + n) % size)
    }
    def take(n: Int): Seq[Int] = string.take(n)
    def insert(s: Seq[Int]) =
      Circle(s ++ string.drop(s.size), offset).rotate(s.size)
    def size = string.size
    def result: Circle = Circle(rotate(size - (offset % size)).string, 0)
    def answer: Int = result.string.take(2).reduce(_ * _)
    def denseHash = result.string.grouped(16).map { case l => l.reduce(_ ^ _) }
  }

  case class Game(skip: Int, circle: Circle) {
    def add(n: Int): Game = {
      Game(skip + 1, circle.insert(circle.take(n).reverse).rotate(skip))
    }
    def result = Game(skip, circle.result)
    def play(l: Seq[Int]): Game = {
      if (l.isEmpty) this else add(l.head).play(l.tail)
    }
  }

  def toHex(l: Seq[Int]) = l.map(a => f"$a%02x").mkString

  def initGame = new Game(0, Circle(string, 0))
  val endGame = initGame.play(source)

  // part 2:
  println(toHex(endGame.circle.denseHash.toSeq))
}
