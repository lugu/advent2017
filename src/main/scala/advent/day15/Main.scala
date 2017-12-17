package advent.day15
import scala.io.Source

object Main extends App {
  // Generator A starts with 618
  // Generator B starts with 814
  val generatorA = Generator(618, 16807, 4)
  val generatorB = Generator(814, 48271, 8)

  // val generatorA = Generator(65, 16807, 4)
  // val generatorB = Generator(8921, 48271, 8)

  val modulo: BigInt = 2147483647
  val mask: BigInt = (1 << 16) - 1

  println(f"$mask%x")

  case class Generator(previous: BigInt, factor: BigInt, multiple: BigInt) {
    def nextMultiple: Generator = {
      val a = next
      if (a.previous % multiple == 0) a else a.nextMultiple
    }
    def next = Generator(previous * factor % modulo, factor, multiple)
    def bits = previous & mask
  }

  def juge(a: Generator,
           b: Generator,
           n: BigInt = 40 * 1000 * 1000,
           score: BigInt = 0): BigInt = {
    if (n == 0) score
    else {
      val nextScore = if (a.bits == b.bits) score + 1 else score
      juge(a.nextMultiple, b.nextMultiple, n - 1, nextScore)
    }
  }
  println(juge(generatorA, generatorB, 5 * 1000 * 1000))
}
