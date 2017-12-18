package advent.day17
import scala.io.Source
object Main extends App {
  case class Spin(offset: Int, size: Int = 1, step: Int = 0, pos: Int = 0, afterZero: Int = -1) {
    def next: Spin = {
      val nextPost = (pos + offset) % size
      val nextAfterZero = if (nextPost == 0) step + 1 else afterZero
      Spin(offset, size + 1, step + 1, nextPost + 1, nextAfterZero)
    }
  }
  def next(n: Int, spin: Spin): Spin = if (n == 0) spin else next(n - 1, spin.next)
  val result = next(50 * 1000 * 1000, Spin(363))
  println(result.afterZero)
}
