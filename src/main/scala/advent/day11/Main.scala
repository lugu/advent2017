package advent.day11
import scala.io.Source

object Main extends App {

  trait Direction
  object N extends Direction { override def toString = "N" }
  object NE extends Direction { override def toString = "NE" }
  object NW extends Direction { override def toString = "NW" }

  // def input: String = "n,n,n"
  // def input: String = "n,ne,n"
  // def input: String = "n,s"
  // def input: String = "n,nw,ne"
  // def input: String = "n,sw,se"
  // def input: String = "s,ne,ne,se"
  def input: String = Source.fromResource("day11.txt").getLines.toList.head
  def source: Seq[Tuple2[Direction,Int]] = input.split(",").map {
    case "n" => (N, 1)
    case "s" => (N, -1)
    case "ne" => (NE, 1)
    case "nw" => (NW, 1)
    case "se" => (NW, -1)
    case "sw" => (NE, -1)
  }

  val group: Map[Direction,Int] = source.groupBy(_._1).mapValues(_.map(_._2).sum)
  def grouped(d: Direction) = group.getOrElse(d, 0)

  
  import Math.abs
  val rules: List[Map[Direction,Int]] = List(Map((N -> -1), (NE -> 1), (NE -> 1)), Map((N -> 1), (NE -> -1), (NW -> -1)))

  def update(r: Map[Direction,Int], group: Map[Direction,Int]): Map[Direction,Int] = {
      group ++ r.map{ case (k,v) => k -> (v + group.getOrElse(k,0)) }
  }

  def steps(group: Map[Direction,Int]) = group.values.map(abs(_)).sum

  def simplify(rules: List[Map[Direction,Int]], group: Map[Direction,Int]): Map[Direction,Int] = {
    if (rules.isEmpty) group else {
      val r = rules.head
      val updated = update(r, group)
      if (steps(updated) < steps(group)) simplify(rules, updated)
      else simplify(rules.tail, group)
    }
  }

  val result = simplify(rules, group)

  println(group)
  println(result)
  println(steps(result))

  // rules: 
  // N ~ S ^^ I
  // NE ~ SW ^^ I
  // NW ~ SE ^^ I
}
