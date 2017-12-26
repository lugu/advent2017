package advent.day23

object Main extends App {

  val debug = true
  val verbose = true
  var verboseNb = 0

  type Instruction = State => State

  case class ProgEnd(p: Prog) extends Exception
  case class InvalidPC(s: State) extends Exception

  case class Regs(s: Array[BigInt]) {
    override def toString = ('a' to 'h').zip(s).mkString
    def read(reg: Char): BigInt = {
      if (reg < 'a' || reg > 'h') throw new Exception(s"reading $reg")
      s(reg - 'a')
    }
    def write(reg: Char, v: BigInt): Regs = {
      if (reg < 'a' || reg > 'h') throw new Exception(s"writting $reg")
      Regs(s.updated(reg - 'a', v))
    }
  }
  object Regs {
    val z: BigInt = 0
    val o: BigInt = 1
    def zero: Regs = Regs(('a' to 'h').map(c => if (c == 'a') o else z).toArray)
  }

  case class State(r: Regs, pc: Int) {
    override def toString = r.toString + s"(pc,$pc)"
    def read(reg: Char): BigInt = r.read(reg)
    def write(reg: Char, v: BigInt) = State(r.write(reg, v), pc)
    def incrPC: State = State(r, pc + 1)
    def setPC(n: BigInt) = State(r, n.toInt)
    def step(i: Seq[Instruction]): State = try {
      i(pc)(this)
    } catch {
      case e: java.lang.IndexOutOfBoundsException => throw new InvalidPC(this)
    }

    def setInst(r: Char, v: BigInt): State = write(r, v)
    def mulInst(r: Char, v: BigInt): State = write(r, v * read(r))
    def jnzInst(t: BigInt, v: BigInt): State =
      if (t == 0) this else setPC(pc + v - 1)
    def subInst(r: Char, v: BigInt): State = write(r, read(r) - v)
  }

class InstructionParser extends scala.util.parsing.combinator.RegexParsers {
    def value: Parser[Int] = "(-?\\d+)".r ^^ { case p => p.toInt }
    def register: Parser[Char] = "([a-z])".r ^^ {
      case n => n.toCharArray.head
    }
    def valueOrRegister: Parser[Either[Char, Int]] =
      (value | register) ^^ (b =>
        b match {
          case c: Char => Left(c)
          case i: Int => Right(i)
        })

    def makeInstructionParser(
        name: String,
        inst: (Char, BigInt) => Instruction): Parser[Instruction] =
      name ~> register ~ valueOrRegister ^^ {
        case (a ~ b) =>
          b match {
            case Left(c) =>
              (s: State) =>
                inst(a, s.read(c))(s)
            case Right(i) =>
              (s: State) =>
                inst(a, i)(s)
          }
      }

    def set: Parser[Instruction] =
      makeInstructionParser(
        "set",
        (r: Char, v: BigInt) => (state: State) => state.setInst(r, v))

    def mul: Parser[Instruction] =
      makeInstructionParser(
        "mul",
        (r: Char, v: BigInt) => (state: State) => state.mulInst(r, v))

    def sub: Parser[Instruction] =
      makeInstructionParser(
        "sub",
        (r: Char, v: BigInt) => (state: State) => state.subInst(r, v))


    def jnz: Parser[Instruction] =
      "jnz" ~> valueOrRegister ~ valueOrRegister ^^ {
        case Right(a) ~ Right(b) =>
          (s: State) =>
            s.jnzInst(a, b)
        case Left(a) ~ Right(b) =>
          (s: State) =>
            s.jnzInst(s.read(a), b)
        case Left(a) ~ Left(b) =>
          (s: State) =>
            s.jnzInst(s.read(a), s.read(b))
        case Right(a) ~ Left(b) =>
          (s: State) =>
            s.jnzInst(a, s.read(b))
      }

    def instruction =
      set | mul | jnz | sub

    def analyse(input: String): Instruction = {
      parse(instruction, input) match {
        case Success(matched, next) => {
          if (!next.atEnd) throw new Exception("Parsed up to: " + next.offset)
          matched
        }
        case Failure(msg, next) => throw new Exception("Failure: " + msg)
        case Error(msg, next) => throw new Exception("Error: " + msg)
      }
    }
  }

  case class Prog(state: State,
                  instructions: Seq[Instruction]) {

    def instString = try {
      input.toSeq(state.pc)
    } catch {
      case e: java.lang.IndexOutOfBoundsException => "No more instruction"
    }
    override def toString =
      s"Instruction: $instString, $state"

    def step: Prog = try {
      verboseNb += 1
      if (verbose && verboseNb % 1000000 == 0) {
        println(this)
      }
      if (debug) {
        println(this)
        if (readLine() != "") 
          throw new ProgEnd(this)
      }
      Prog(state.step(instructions).incrPC, instructions)
    } catch {
      case InvalidPC(s) => throw new ProgEnd(this)
    }

    @scala.annotation.tailrec
    final def runLoop: Prog = step.runLoop

    def run: Prog = try {
      runLoop
    } catch {
      case ProgEnd(p) => return p
    }
  }

  def parser = (new InstructionParser)
  def input = scala.io.Source.fromResource("day23.txt").getLines
  def instructions: Seq[Instruction] = input.map(parser.analyse(_)).toSeq
  val prog = Prog(State(Regs.zero, 0), instructions)

  def part1() {
    println(prog.run)
  }

  def isPrime(i: Int): Boolean = {
    if (i <= 1) false
    else if (i == 2) true
    else !(2 to (i-1)).exists(x => i % x == 0)
  }
  def part2() = {
    println((0 to 1000).map(i => (17 * i) + 109900).filter(!isPrime(_)).size)
  }
  part2()
}
