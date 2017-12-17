package advent.day14
import scala.io.Source

object Main extends App {
  val input = "wenycdww"
  val source = (0 to 127).map(input + "-" + _.toString)

  import advent.day10.Main.{knotHash, toHex}
  def knotHashBits(s: String) =
    toHex(knotHash(s)).toSeq.map(c => bits(fromHexString(c.toString))).flatten
  def fromHexString(s: String): Byte = Integer.parseInt(s, 16).toByte
  def bits(byte: Byte) = (0 to 3).map(bit => (byte & (1 << bit)) != 0).reverse

  def bitsList: Seq[Seq[Boolean]] = source.map(knotHashBits(_))

  import advent.day03.Main.Pos
  def bitsMap: Map[Pos, Boolean] =
    bitsList.zipWithIndex
      .flatMap {
        case (row, i) => {
          row.zipWithIndex.map {
            case (bool, j) => (Pos(i, j), bool)
          }
        }
      }
      .filter(_._2)
      .toMap

  def bitsGraph(m: Map[Pos, Boolean]): Map[Int, List[Int]] = m.map {
    case (pos, _) => {
      val neighboors: List[Pos] =
        pos.adjacentClose.filter(p => m.isDefinedAt(p) && p != pos).toList
      (pos.flatIndex(128), neighboors.map(_.flatIndex(128)))
    }
  }

  def testGraph(g: Map[Int, List[Int]]) = g.foreach {
    case (k, l) =>
      l.foreach(p =>
        if (!g(p).contains(k)) throw new Exception("invalid node " + k))
  }

  val graph = bitsGraph(bitsMap)
  // val graph = Map((1 -> List(2)), (2 -> List(1, 3, 7)), (3 -> List(2, 8)), (4 -> List(5)), (5 -> List(4)), (6 -> List()), (7 -> List(2, 8)), (8 -> List(7, 3)))
  println(graph.take(5).mkString("\n"))
  println(graph.size)
  testGraph(graph)

  import advent.day12.Main.groupConnected
  val result = groupConnected(graph)
  println(result.map(_.take(10)).mkString("\n"))
  println(result.size)
}
