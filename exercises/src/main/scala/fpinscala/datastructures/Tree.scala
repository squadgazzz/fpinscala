package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // a bad one
  //  def size[A](t: Tree[A]): Int = t match {
  //    case Leaf(_) => 1
  //    case Branch(l, r) => 1 + size(l) + size(r)
  //  }

  def size[A](t: Tree[A]): Int = {
    def inner_size(list: scala.collection.immutable.List[Tree[A]],
                   acc: Int): Int =
      list match {
        case scala.collection.immutable.Nil => acc
        case Leaf(_) :: xs                  => inner_size(xs, acc + 1)
        case Branch(l, r) :: xs             => inner_size(l :: r :: xs, acc + 1)
      }

    inner_size(scala.collection.immutable.List(t), 0)
  }

  // a bad one more
  //  def max(t: Tree[Int]): Int = t match {
  //    case Leaf(v)      => v
  //    case Branch(l, r) => max(l) max max(r)
  //  }

  def max(t: Tree[Int]): Int = {
    def go(list: scala.collection.immutable.List[Tree[Int]], acc: Int): Int =
      list match {
        case scala.collection.immutable.Nil => acc
        case Leaf(v) :: xs                  => go(xs, v max acc)
        case Branch(l, r) :: xs             => go(l :: r :: xs, acc)
      }
    go(scala.collection.immutable.List(t), 0)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(10), Leaf(20)), Branch(Leaf(90), Leaf(1)))
    println(size(tree))
    println(max(tree))
  }
}
