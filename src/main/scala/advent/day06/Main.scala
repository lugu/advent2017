package advent.day06

import scala.io.Source

object Main extends App {

  var step = 0

  // def source = Array(2, 4, 1, 2)
  def source =
    Source
      .fromResource("day06.txt")
      .getLines
      .toList
      .head
      .split("\\s+")
      .map(_.toInt)
      .toArray

  def beep(since: Int, until: Int, size: Int) =
    (0 to since - 1).map(a => 0) ++
      (since to until).map(a => 1) ++
      (until + 1 to size - 1).map(a => 0)

  def distribute(banks: Array[Int], since: Int, max: Int): Array[Int] = {

    if (max == 0) return banks

    val toWrite =
      if (max <= (banks.size - 1 - since)) max else (banks.size - since)
    val until = since + toWrite - 1

    lazy val toAdd = beep(since, until, banks.size)
    lazy val newBanks = banks.zip(toAdd).map { case (a, b) => a + b }

    distribute(newBanks, 0, max - toWrite)
  }

  def loop(banks: Array[Int], zeros: List[Array[Int]]): Int = {

    step = step + 1

    // println("banks: ", banks.toVector.toString)

    val max = banks.max
    val maxIndex = banks.indexWhere(_ == max)
    val newBanks = banks.zipWithIndex.map {
      case (n, i) => if (i == maxIndex) 0 else n
    }.toArray

    val since = if (maxIndex == banks.size - 1) 0 else maxIndex + 1

    val distributedBanks = distribute(newBanks, since, max)
    if (zeros.exists(_.deep == distributedBanks.deep)) step + 1
    else loop(distributedBanks, distributedBanks :: zeros)
  }

  println(loop(source, List(source)))
}
