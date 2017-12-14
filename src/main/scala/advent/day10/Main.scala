package advent.day10
import scala.io.Source

object Main extends App {

  // val string = (0 to 4).toList
  // val source = List(3, 4, 1, 5)
  val string = (0 to 255).toList
  val source = Source
    .fromResource("day10.txt")
    .getLines
    .map(t => t.split(","))
    .flatten
    .map(_.toInt)
    .toList

  case class Circle(string: Seq[Int], offset: Int) {
    def rotate(n: Int) =
      Circle(string.drop(n) ++ string.take(n), (offset + n) % size)
    def take(n: Int): Seq[Int] = string.take(n)
    def insert(s: Seq[Int]) =
      Circle(s ++ string.drop(s.size), offset).rotate(s.size)
    def size = string.size
    def answer: Int = {
      val rot = size - (offset % size)
      println(size)
      println(offset)
      println(rot)
      rotate(rot).take(2).reduce(_ * _)
    }
  }
  case class Game(skip: Int, circle: Circle) {
    def add(n: Int): Game = {
      val g =
        Game(skip + 1, circle.insert(circle.take(n).reverse).rotate(skip))
      println(g)
      g
    }
    def play(l: Seq[Int]): Game = l match {
      case List() => this
      case a :: rest => add(a).play(rest)
    }
  }

  val initGame = new Game(0, Circle(string, 0))
  println(initGame)
  val endGame = initGame.play(source)
  // println(endGame)
  println(endGame.circle.answer)
}
