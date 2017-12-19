package advent.day18

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens

object Main extends App {

  type Instruction = State => State

  object ReceiveInterrupt extends Exception
  case class PendingInstruction(s: State) extends Exception

  case class Bus(in: List[BigInt], out: List[BigInt]) {
    def connect(o: Bus): Tuple2[Bus,Bus] = {
      (Bus(in ++ o.out, List()), Bus(o.in ++ out, List()))
    }
    def send(v: BigInt) = Bus(in, out :+ v)
    def receive: Tuple2[BigInt,Bus] = in match {
      case List() => throw ReceiveInterrupt
      case a :: rest => (a, Bus(rest, out))
    }
  }

  @scala.annotation.tailrec
  def runLoop(s: State, i: Seq[Instruction]): State = runLoop(s.step(i), i)

  case class State(s: Map[Char, BigInt], pc: Int, bus: Bus) {
    def read(reg: Char): BigInt = s.getOrElse(reg, 0)
    def write(reg: Char, v: BigInt) = State(s + (reg -> v), pc, bus)
    def p: Int = s('p').toInt
    def next: State = State(s, pc + 1, bus)
    def send(a: BigInt) = State(s, pc, bus.send(a))
    def receive: Tuple2[BigInt,State] = {
      val (a, b) = bus.receive
      (a, State(s, pc, b))
    }
    def setPC(n: BigInt) = State(s, n.toInt, bus)

    def stepSafe(i: Seq[Instruction]): State = try {

      // println(input.toSeq(pc))
      // println(this)
      // val a = scala.io.StdIn.readLine()
      // if (a != "") throw new Exception("Halted")

      step(i)
    } catch {
      case PendingInstruction(state) => state
    }
    def step(i: Seq[Instruction]): State = try {
      i(pc)(this).next
    } catch {
      case ReceiveInterrupt => throw new PendingInstruction(this)
    }
    def run(i: Seq[Instruction]): State = try {
      runLoop(this, i)
    } catch {
      case PendingInstruction(state) => state
    }
    def isEqual(o: State): Boolean = (pc == o.pc && (s.toSet diff o.s.toSet).isEmpty)
    def setBus(b: Bus): State = State(s, pc, b)
    def updateBus(o: State): Tuple2[State,State] = {
      val (nextBusA, nextBusB) = bus.connect(o.bus)
      (setBus(nextBusA), o.setBus(nextBusB))
    }
  }


  class InstructionParser extends RegexParsers {
    def value: Parser[Int] = "(-?\\d+)".r ^^ { case p => p.toInt }
    def register: Parser[Char] = "(\\w)".r ^^ { case n => n.toCharArray.head }
    def valueOrRegister: Parser[Either[Char,Int]] = (value | register) ^^ (b => b match {
      case c: Char => Left(c)
      case i: Int => Right(i)
    })

    def twoOperandInst(op: String, f: (Char, BigInt) => Instruction) =
      op ~> register ~ valueOrRegister ^^ {
        case (a ~ b) => (s: State) => b match {
          case Left(c) => f(a, s.read(c))(s)
          case Right(i) => f(a, i)(s)
        }
      }

    def set: Parser[Instruction] =
      twoOperandInst("set", {
        case (r, v) =>
          (s: State) =>
            s.write(r, v)
      })
    def mul: Parser[Instruction] =
      twoOperandInst("mul", {
        case (r, v) =>
          (s: State) =>
            s.write(r, v * s.read(r))
      })
    def jmp: Parser[Instruction] =
      twoOperandInst("jgz", {
        case (r, v) =>
          (s: State) =>
            if (s.read(r) <= 0) s else s.setPC(s.pc + v - 1)
      })
    def add: Parser[Instruction] =
      twoOperandInst("add", {
        case (r, v) =>
          (s: State) =>
            s.write(r, v + s.read(r))
      })
    def mod: Parser[Instruction] =
      twoOperandInst("mod", {
        case (r, v) =>
          (s: State) =>
            s.write(r, s.read(r) % v)
      })
    def snd: Parser[Instruction] =
      "snd" ~> register ^^ (r => (s: State) => s.send(s.read(r)))
    def rcv: Parser[Instruction] =
      "rcv" ~> register ^^ (r =>
        (s: State) =>
          if (s.read(r) != 0) {
            val (value, state) = s.receive
            state.write(r, value)
          } else s)
    def instruction =
      set | mul | jmp | add | mod | snd | rcv

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

  def parser = (new InstructionParser)
  def input = Source.fromResource("day18.txt").getLines
  // def input = "set a 1; add a 2; mul a a; mod a 5; snd a; set a 0; rcv a; jgz a -1; set a 1; jgz a -2".split(";")
  def instructions: Seq[Instruction] = input.map(parser.analyse(_)).toSeq

  val emptyBus = Bus(List(), List())
  val a = State(Map(('p' -> 0)), 0, emptyBus)
  val b = State(Map(('p' -> 1)), 0, emptyBus)

  def loop(a: State, b: State): Tuple2[State,State] = {
    val (nextA, nextB) = a.stepSafe(instructions).updateBus(b.stepSafe(instructions))
    if (a.isEqual(nextA) && b.isEqual(nextB)) (a, b)
    else loop(nextA, nextB)
  }

  val (finalA, finalB) = loop(a, b)
  println(finalA)
  println(finalB)
}
