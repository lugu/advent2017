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

  println(source.map(s => knotHashBits(s).filter(_ == true).size).sum)
}
