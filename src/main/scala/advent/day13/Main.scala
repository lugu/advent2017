package advent.day13

import scala.io.Source

object Main extends App {

  case class Layer(depth: Int, range: Int) {
    def position(time: Int): Int = {
      val tripDuration = (2 * range) - 2
      val tripPosition = time % tripDuration
      val position =
        if (tripPosition < range) tripPosition else (2 * range - tripPosition)
      position
    }
    def isDetected(time: Int) = (position(time) == 0)
    def severity = range * depth + 1
    def score(time: Int) =
      if (isDetected(time)) {
        severity
      } else 0
  }

  // def input = "0: 3;1: 2;4: 4;6: 4".split(";")
  def input = Source.fromResource("day13.txt").getLines

  // sort the layers by range to increase caught detection probability
  def firewall: Seq[Layer] =
    input
      .map(_.split(": ").map(_.toInt))
      .map { case Array(a, b) => Layer(a, b) }
      .toSeq
      .sortBy(_.range)

  def scoreAt(delay: Int) = {
    firewall.map(layer => layer.score(layer.depth + delay)).sum
  }
  def loopDetection(delay: Int): Int = {
    firewall.find(layer => layer.isDetected(layer.depth + delay)) match {
      case None => delay
      case _ => loopDetection(delay + 1)
    }
  }
  println(loopDetection(0))
}
