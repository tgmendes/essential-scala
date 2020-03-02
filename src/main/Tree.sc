object main {

  sealed trait Tree {
    def sum(tree: Tree): Int = tree match {
      case Leaf(elt) => elt
      case Node(l, r) => l.sum + r.sum
    }

  }


  final case class Node(l: Tree, r: Tree) extends Tree

  final case class Leaf(elt: Int) extends Tree

}