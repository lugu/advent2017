package advent.day25

object Main extends App {

  case class Mem(m: Map[Int,Int], c: Int) {
    def read: Int = m.getOrElse(c, 0)
    def write(i: Int): Mem = Mem(m + (c -> i), c)
    def left: Mem = Mem(m, c - 1)
    def right: Mem = Mem(m, c + 1)
    def checksum: Int = m.values.sum
  }

  case class State(switch0: Boolean, left0: Boolean, switch1: Boolean, left1: Boolean, state0: Char, state1: Char) {

    def step(mem: Mem): Mem = {
      val newMem = mem.read match {
      case 0 => if (switch0) mem.write(1) else mem
      case 1 => if (switch1) mem.write(0) else mem
      }
      mem.read match {
      case 0 => if (left0) newMem.left else newMem.right
      case 1 => if (left1) newMem.left else newMem.right
      }
    }
    def next(mem: Mem): State = mem.read match {
      case 0 => state(state0)
      case 1 => state(state1)
    }
    def state(c: Char) = states(c)
  }

  val states: Map[Char,State] = Map(
    ('A' -> State(true, false, true, true, 'B', 'E')),
    ('B' -> State(true, true, true, false, 'C', 'A')),
    ('C' -> State(true, true, true, false, 'D', 'Z')),
    ('Z' -> State(true, true, true, false, 'D', 'C')),
    ('D' -> State(true, true, true, true, 'E', 'F')),
    ('E' -> State(true, true, false, true, 'A', 'C')),
    ('F' -> State(true, true, false, false, 'E', 'A')))

  def diagnostic(steps: Int, state: State, mem: Mem): Int = {
    if (steps == 0) mem.checksum
    else diagnostic(steps -1, state.next(mem), state.step(mem))
  }

  println(states('A'))
  println(diagnostic(12386363, states('A'), Mem(Map(), 0)))
}
