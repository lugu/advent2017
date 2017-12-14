package advent.day02
import scala.io.Source

object Main extends App {
  val source =
    Source.fromResource("day02.txt").getLines.map(t => t.split("\\s+")).toList
  val size = source
    .map(_.foldLeft(Option("")) {
      case (option, word) =>
        option match {
          case None => None
          case Some(pass) =>
            if (pass
                  .split(" ")
                  .exists(w => w.toList.sorted == word.toList.sorted)) None
            else Some(pass + " " + word)
        }
    })
    .filter(_ != None)
    .size
  println(size)
}
