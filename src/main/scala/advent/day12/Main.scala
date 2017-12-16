package advent.day12

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens

trait SimpleTokens extends Tokens {
  trait Symbol extends Token
  case class Pid(p: Int) extends Symbol { def chars = toString }
  case class Prog(pid: Int, pipes: Seq[Int]) extends Symbol {
    def chars = toString
  }
}

class PipesCollector extends RegexParsers with SimpleTokens {
  def pid: Parser[Int] = raw"(\d+)".r ^^ { case p => p.toInt }
  def pidSeq: Parser[Seq[Int]] = rep1(pid, "," ~> pid)
  def prog: Parser[Prog] = pid ~ "<->" ~ pidSeq.? ^^ {
    case p ~ s ~ seq => Prog(p, seq.getOrElse(Seq()))
  }
  def analyse(input: String): Prog = {
    parse(prog, input) match {
      case Success(matched, next) => {
        if (!next.atEnd) throw new Exception("Parsed up to: " + next.offset)
        return matched
      }
      case Failure(msg, next) =>
        throw new Exception(
          "GroupConstructor Failure: " + msg + " at " + next.offset)
      case Error(msg, next) =>
        throw new Exception(
          "GroupConstructor Error: " + msg + " at " + next.offset)
    }
  }
}

object Main extends App {

  def input = Source.fromResource("day12.txt").getLines.toSeq
  def parser = new PipesCollector()
  def progs = input.map(parser.analyse(_)).toList
  def graph: Map[Int, List[Int]] =
    progs.groupBy(_.pid).mapValues(_.flatMap(a => a.pipes).distinct)
  def components(visiting: Int,
                 graph: Map[Int, List[Int]],
                 visited: List[Int],
                 connected: List[Int]): List[Int] = {
    val pipes = graph(visiting)
    // for pipes not visited add them to connected and continue:
    val newConnected = (pipes ++ connected).distinct
    val childConnected = pipes
      .filter(!visited.contains(_))
      .map(components(_, graph, visiting :: visited, newConnected))
      .flatten
    (newConnected ++ childConnected).distinct
  }

  def connected(pid: Int, graph: Map[Int, List[Int]]): List[Int] =
    components(pid, graph, List(), List())
  def groupConnected(graph: Map[Int, List[Int]]): List[List[Int]] =
    graph.headOption match {
      case None => List()
      case Some((pid, pipes)) => {
        val headConnected = connected(pid, graph)
        val restGraph = graph.filterKeys(!headConnected.contains(_))
        headConnected :: groupConnected(restGraph)
      }
    }
  println(groupConnected(graph).size)
}
