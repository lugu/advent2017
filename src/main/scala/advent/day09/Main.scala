package advent.day09

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens

object Main extends App {

  val source = Source.fromResource("day09.txt").getLines.mkString
  val solution =
    new GroupConstructor().analyse(new GarbageCollector().analyse(source))
  println(solution)

  // val source = "a<bla>a<bla>c"
  // val source = "a<b!la>a<bl!a>c"
  // val source = "a<bla>b<bla <bla!>>c"
  // val source = "a<<!!!>z<bla>b<bla <bla!>>c"
  // val source = "<bla><bla <bla>"
  // val source = "{<bla><bla <bla>}"
  // val source = "{e<bla><bla {}<bla>b}{bb}"
  // val source = "{e<bla><bla {}<bla>b{z}}{bb}"

  val garbage = new GarbageCollector().analyseGarbage(source)
  println(garbage)
  println(garbage.size)

  // val source = "{}"
  // val source = "{{},{}}"
  // val source = "{{},{},}"
  // val source = "{{{}},{},}"
  // val source = "{,,{},}"
  // val groups = new GroupConstructor().analyse(source)
  // println(groups)
}

trait SimpleTokens extends Tokens {
  trait Symbol extends Token

  case class Content(chars: String) extends Symbol
  case class Group(contents: List[Token]) extends Symbol {
    def subgroups = contents.collect { case g: Group => g }
    def score(level: Int): Int =
      (level :: subgroups.map(_.score(level + 1))).sum
    def chars = score(1).toString
  }

  object Coma extends Symbol { def chars = "," }
  object Empty extends Symbol { def chars = "o" }
  object Exclamation extends Symbol { def chars = "!" }
  case class Garbage(chars: String) extends Symbol
}

class GarbageCollector extends RegexParsers with SimpleTokens {
  override def skipWhitespace = false

  def nonGarbage: Parser[Token] = "([^<]+)".r ^^ { case g => Content(g) }

  def garbageContent: Parser[Token] = "([^!>]+)".r ^^ { case g => Content(g) }
  def exclamation: Parser[Token] = "!.".r ^^ { case g => Exclamation }

  def garbage: Parser[Token] =
    "<" ~> ((exclamation | garbageContent) *) <~ ">" ^^ {
      case l =>
        Garbage(l.collect { case c: Content => c.chars }.mkString)
    }

  def tokens = (nonGarbage | garbage) *

  def analyse(input: String): String = {
    parse(tokens, input) match {
      case Success(matched, next) => {
        if (!next.atEnd) throw new Exception("Parsed up to: " + next.offset)
        matched.collect { case a: Content => a.chars }.mkString
      }
      case Failure(msg, next) => {
        throw new Exception("Failure: " + msg)
      }
      case Error(msg, next) => {
        throw new Exception("Error: " + msg)
      }
    }
  }
  def analyseGarbage(input: String): String = {
    parse(tokens, input) match {
      case Success(matched, next) => {
        if (!next.atEnd) throw new Exception("Parsed up to: " + next.offset)
        matched.collect { case a: Garbage => a.chars }.mkString
      }
      case Failure(msg, next) => {
        throw new Exception("Failure: " + msg)
      }
      case Error(msg, next) => {
        throw new Exception("Error: " + msg)
      }
    }
  }
}

class GroupConstructor extends RegexParsers with SimpleTokens {
  override def skipWhitespace = false

  def coma: Parser[Token] = "," ^^ (_ => Coma)
  def element: Parser[Token] = group.? ^^ {
    case Some(g) => g
    case None => Empty
  }
  def content: Parser[Token] = rep1(element, coma ~> element) ^^ (Group(_))
  def group: Parser[Token] = "{" ~> content.? <~ "}" ^^ {
    case Some(t) => t
    case None => Group(List())
  }

  def tokens = group

  def analyse(input: String): Token = {
    parse(tokens, input) match {
      case Success(matched, next) => {
        if (!next.atEnd) throw new Exception("Parsed up to: " + next.offset)
        return matched
      }
      case Failure(msg, next) => {
        throw new Exception(
          "GroupConstructor Failure: " + msg + " at " + next.offset)
      }
      case Error(msg, next) => {
        throw new Exception(
          "GroupConstructor Error: " + msg + " at " + next.offset)
      }
    }
  }
}
