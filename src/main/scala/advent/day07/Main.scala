package advent.day07

import scala.io.Source

case class Prog(name: String, size: Int, childs: Seq[String])

object Prog {
  val ProgRegexp = raw"(\w+)\s*\((\d+)\)[\s->]*(.*)".r

  def apply(l: String): Prog = l match {
    case ProgRegexp(name, size, child) =>
      Prog(name, size.toInt, child.split(",\\s+").filter(_ != ""))
  }
}

object Main extends App {

  // mmqyju (156) -> rjzvwv, noybkx
  // dvkug (90) -> tbjbz, gxnvl
  // meeiw (95)

  def find(name: String): Prog = {
    source.find(_.name == name) match {
      case Some(p) => p
      case _ => {
        println(name)
        Prog("", 0, List())
      }
    }
  }

  lazy val source: Seq[Prog] =
    Source.fromResource("day07.txt").getLines.toList.map(Prog(_))

  def parent(l: Seq[Prog], myParentIs: Map[Prog, Prog]): Map[Prog, Prog] =
    l match {
      case List() => myParentIs
      case head :: tail =>
        if (head.childs.isEmpty) parent(tail, myParentIs)
        else
          parent(tail, head.childs.foldLeft(myParentIs) {
            case (map, name) => map + (find(name) -> head)
          })
    }

  source.take(20).foreach { p: Prog =>
    println(p.toString)
  }

  val myParentIs: Map[Prog, Prog] = parent(source, Map())
  val root = source.find(b => !myParentIs.keys.exists(a => a == b)).get

  def getWeight(p: Prog): Int = {
    if (p.childs.isEmpty) p.size
    else {
      val sizes = p.childs.map(find(_)).map(getWeight(_))
      if (sizes.min != sizes.max) {
        println("!!! unbalanced")
        println(p.childs)
        println(sizes)
        println(find("sphbbz"))
      }
      sizes.foldLeft(p.size)(_ + _)
    }
  }
  println(root)
  println(getWeight(root))
}
