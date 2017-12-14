package advent.day08
import scala.io.Source

object Main extends App {

  // b inc 5 if a > 1
  // a inc 1 if b < 5
  // c dec -10 if a >= 1
  // c inc -20 if c == 10

  case class State(s: Map[String, Int], max: Int) {
    def read(reg: String): Int = s.getOrElse(reg, 0)
    def write(reg: String, v: Int) = {
      State(s + (reg -> v), if (v > max) v else max)
    }
  }

  type Instruction = State => State
  type Condition = State => Boolean

  // val input = List("b inc 5 if a > 1", "a inc 1 if b < 5")
  val input = Source.fromResource("day02.txt").getLines.toList

  val InstructionR = raw"(\w+) (\w+) (-?\d+) if (\w+) ([!<=>]+) (-?\d+)".r

  case class Operation(dest: String,
                       op: String,
                       delta: Int,
                       sourceCond: String,
                       opCond: String,
                       v: Int) {
    def condition: Condition = {
      case (s: State) =>
        opCond match {
          case "<" => (s.read(sourceCond) < v)
          case "<=" => (s.read(sourceCond) <= v)
          case ">" => (s.read(sourceCond) > v)
          case ">=" => (s.read(sourceCond) >= v)
          case "==" => (s.read(sourceCond) == v)
          case "!=" => (s.read(sourceCond) != v)
        }
    }
    def instruction: Instruction = {
      case (s: State) =>
        if (!condition(s)) s
        else {
          op match {
            case "inc" => s.write(dest, s.read(dest) + delta)
            case "dec" => s.write(dest, s.read(dest) + (-1) * delta)
          }
        }
    }
  }

  object Operation {
    def apply(s: String): Operation = s match {
      case InstructionR(dest, op, delta, sourceCond, opCond, v) =>
        Operation(dest, op, delta.toInt, sourceCond, opCond, v.toInt)
    }
  }

  val source: Seq[Instruction] = input.map(Operation(_).instruction)
  val result: State = source.foldLeft(State(Map(), 0)) { case (s, i) => i(s) }
  println(result.max)
}
