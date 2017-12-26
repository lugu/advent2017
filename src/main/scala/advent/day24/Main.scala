package advent.day24

object Main extends App {
  case class Pad(a: Int, b: Int) {
    def reverse = Pad(b, a)
    def score = a + b
    def equals(o: Pad): Boolean = (a == o.a && b == o.b) || (b == o.a && a == o.b)
    def combine(o: Pad): Option[Pad] = if (b == o.a) Some(o) else if (b == o.b) Some(o.reverse) else None
  }
  def input = scala.io.Source.fromResource("day24.txt").getLines
  def source = input.map(s => s.split("/")).map{ case Array(a, b) => Pad(a.toInt, b.toInt) }.toList

  def remove(pads: List[Pad], pad: Pad): List[Pad] = {
    val i = pads.indexWhere(pad.equals(_))
    pads.take(i) ++ pads.drop(i + 1)
  }
  def score(pads: List[Pad]) = pads.map(_.score).sum
  def combine(pads: List[Pad], in: Pad): List[Pad] = {
    val canCombine: List[Pad] = pads.flatMap(in.combine(_))
    val combinaisons: List[List[Pad]] = List(in) :: canCombine.map(pad => in :: combine(remove(pads, pad), pad))
    val length = combinaisons.map(l => l.size).max
    val longest = combinaisons.filter(l => l.size == length)
    val max = longest.map(l => score(l)).max
    longest.find(l => score(l) == max) match {
      case Some(seq) => seq
    }
  }
  val solution = combine(source, Pad(0, 0))
  println(solution.map(_.score).sum)
}
