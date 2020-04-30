package fpinscala.datastructures

object TreeTest extends App {

  val tree = Branch(Leaf(1), Leaf(2))
  println(s"size = ${Tree.size(tree)}")
}
