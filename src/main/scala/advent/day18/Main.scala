package advent.day18

import scala.io.Source
import scala.collection.immutable.Queue
import scala.util.parsing.combinator.RegexParsers

object Main extends App {

  type Instruction = State => State

  object ReceiveInterrupt extends Exception
  case class PendingInstruction(p: Prog) extends Exception

  case class Pipe(in: Queue[BigInt] = Queue(), out: Queue[BigInt] = Queue()) {
    def connect(o: Pipe): Tuple2[Pipe, Pipe] = {
      (Pipe(in ++ o.out, Queue()), Pipe(o.in ++ out, Queue()))
    }
    def send(v: BigInt) = Pipe(in, out :+ v)
    def receive: Tuple2[BigInt, Pipe] =
      if (in.isEmpty) throw ReceiveInterrupt
      else (in.head, Pipe(in.tail, out))
  }

  case class State(s: Map[Char, BigInt], pc: Int, pipe: Pipe) {
    def read(reg: Char): BigInt = s.getOrElse(reg, 0)
    def write(reg: Char, v: BigInt) = State(s + (reg -> v), pc, pipe)
    def p: Int = s('p').toInt
    def incrPC: State = State(s, pc + 1, pipe)
    def send(a: BigInt) = State(s, pc, pipe.send(a))
    def receive: Tuple2[BigInt, State] = {
      val (a, b) = pipe.receive
      (a, State(s, pc, b))
    }
    def setPC(n: BigInt) = State(s, n.toInt, pipe)
    def step(i: Seq[Instruction]): State = i(pc)(this)
    def setPipe(b: Pipe): State = State(s, pc, b)

    def setInst(r: Char, v: BigInt): State = write(r, v)
    def mulInst(r: Char, v: BigInt): State = write(r, v * read(r))
    def jgzInst(t: BigInt, v: BigInt): State =
      if (t <= 0) this else setPC(pc + v - 1)
    def addInst(r: Char, v: BigInt): State = write(r, v + read(r))
    def modInst(r: Char, v: BigInt): State = write(r, read(r) % v)

    def sndInst(v: BigInt): State = send(v)
    def rcvInst(r: Char): State = {
      val (value, state) = receive
      state.write(r, value)
    }
  }

  class InstructionParser extends RegexParsers {
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

    def add: Parser[Instruction] =
      makeInstructionParser(
        "add",
        (r: Char, v: BigInt) => (state: State) => state.addInst(r, v))

    def mod: Parser[Instruction] =
      makeInstructionParser(
        "mod",
        (r: Char, v: BigInt) => (state: State) => state.modInst(r, v))

    def snd: Parser[Instruction] = "snd" ~> valueOrRegister ^^ {
      case Left(r) =>
        (s: State) =>
          s.sndInst(s.read(r))
      case Right(i) =>
        (s: State) =>
          s.sndInst(i)
    }

    def rcv: Parser[Instruction] =
      "rcv" ~> register ^^ (r => (s: State) => s.rcvInst(r))

    def jgz: Parser[Instruction] =
      "jgz" ~> valueOrRegister ~ valueOrRegister ^^ {
        case Right(a) ~ Right(b) =>
          (s: State) =>
            s.jgzInst(a, b)
        case Left(a) ~ Right(b) =>
          (s: State) =>
            s.jgzInst(s.read(a), b)
        case Left(a) ~ Left(b) =>
          (s: State) =>
            s.jgzInst(s.read(a), s.read(b))
        case Right(a) ~ Left(b) =>
          (s: State) =>
            s.jgzInst(a, s.read(b))
      }

    def instruction =
      set | mul | jgz | add | mod | snd | rcv

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

  @scala.annotation.tailrec
  def runLoop(p: Prog, steps: Int = 10000): Prog =
    if (steps == 0) p else runLoop(p.step, steps - 1)

  var transmitted = 0

  case class Prog(state: State,
                  instructions: Seq[Instruction],
                  pending: Boolean = false) {

    def isBlocked: Boolean = pending && state.pipe.in.isEmpty
    def setPipe(b: Pipe) = Prog(state.setPipe(b), instructions, pending)

    def pipe(o: Prog): Tuple2[Prog, Prog] = {

      transmitted += o.state.pipe.out.size
      val ain = state.pipe.in.size
      val bin = o.state.pipe.in.size
      val apc = state.pc
      val bpc = o.state.pc
      println(
        s"transmitted: $transmitted (a in: $ain pc: $apc, b in: $bin pc: $bpc)")

      val (nextPipeA, nextPipeB) = state.pipe.connect(o.state.pipe)
      (setPipe(nextPipeA), o.setPipe(nextPipeB))
    }

    def instString = input.toSeq(state.pc)
    override def toString =
      s"Instruction: $instString (pending $pending) $state"

    def stepOnce: Prog =
      try {
        step
      } catch {
        case PendingInstruction(prog) =>
          Prog(prog.state, prog.instructions, true)
      }

    def step: Prog =
      try {
        Prog(state.step(instructions).incrPC, instructions, false)
      } catch {
        case ReceiveInterrupt => throw new PendingInstruction(this)
      }

    def run: Prog =
      try {
        runLoop(this)
      } catch {
        case PendingInstruction(prog) =>
          Prog(prog.state, prog.instructions, true)
      }
  }

  def input = Source.fromResource("day18.txt").getLines
  // def input = "set a 1; add a 2; mul a a; mod a 5; snd a; set a 0; rcv a; jgz a -1; set a 1; jgz a -2".split(";")
  // def input = "snd 1; snd 2; snd p; rcv a; rcv b; rcv c; rcv d".split(";")

  def parser = (new InstructionParser)
  def instructions: Seq[Instruction] = input.map(parser.analyse(_)).toSeq

  def loop(a: Prog, b: Prog): Tuple2[Prog, Prog] = {
    val progs = List(a, b).par.map(_.run)
    val (nextA, nextB) = progs.head.pipe(progs.tail.head)
    if (nextA.isBlocked && nextB.isBlocked) (nextA, nextB)
    else loop(nextA, nextB)
  }

  def part2 {
    val a = Prog(State(Map(('p' -> 0)), 0, Pipe()), instructions)
    val b = Prog(State(Map(('p' -> 1)), 0, Pipe()), instructions)
    val (finalA, finalB) = loop(a, b)
  }
  part2
}
