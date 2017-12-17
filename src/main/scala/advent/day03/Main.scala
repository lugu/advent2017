package advent.day03
import math.abs

object Main extends App {

  case class Pos(x: Int, y: Int) {
    def isWithin(box: Pos) = (abs(x) <= abs(box.x) && abs(y) <= abs(box.y))
    def add(b: Pos) = Pos(x + b.x, y + b.y)
    def increase = add(Pos(1, 1))
    def move(dir: Pos) = add(dir)
    def adjacent: Seq[Pos] =
      (for (a <- -1 to 1; b <- -1 to 1) yield add(Pos(a, b)))
    def adjacentClose: Seq[Pos] =
      Seq(Pos(x - 1, y), Pos(x + 1, y), Pos(x, y - 1), Pos(x, y + 1))
    def score(cache: Map[Pos, Int]): Int =
      adjacent
        .map(cache.get(_) match {
          case None => 0
          case Some(n) => n
        })
        .reduce(_ + _)
    def flatIndex(base: Int) = x * base + y
  }

  def directions =
    Stream
      .continually(List(Pos(1, 0), Pos(0, 1), Pos(-1, 0), Pos(0, -1)))
      .flatten

  case class Num(i: Int,
                 pos: Pos,
                 box: Pos,
                 dir: Stream[Pos],
                 cache: Map[Pos, Int]) {
    def next: Num = {

      // si je dépasse alors je change de direction sauf si c'est left.
      val (d, p, b) =
        if (pos.move(dir.head).isWithin(box))
          (dir, pos.move(dir.head), box)
        else if (dir.head == Pos(1, 0))
          (dir.tail, pos.move(dir.head), box.increase)
        else
          (dir.tail, pos.move(dir.tail.head), box)

      val s = p.score(cache)
      val c = cache + (p -> s)
      Num(s, p, b, d, c)
    }
    def manathan = abs(pos.x) + abs(pos.y)
  }

  def one = Num(1, Pos(0, 0), Pos(0, 0), directions, Map((Pos(0, 0) -> 1)))

  var next = one
  while (next.i < 361527) {
    next = next.next
  }
  println(next)
}
