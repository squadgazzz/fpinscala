package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val xx = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumL(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def productL(ns: List[Int]) = foldLeft(ns, 1.0)(_ * _)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, xs)        => xs
    case Cons(_, Nil) | Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil        => Cons(h, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Cons(_, t) => drop(t, n - 1)
        case Nil        => l
      }
  }

  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
    println(drop(l, 0))
    println(dropWhile(l)(_ <= 0))
    val ll = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(ll)
    println(length(l))

    println(sumL(l))
    println(productL(l))

    println(reverse(l))
    println(map2(l)(_ * 2))

    val lll = List(List(1, 2, 3), List(3, 4, 5), List(1))
    println(concat(lll))
    println(appendR(l, l))
    println(add1(l))

    val d = List(1.0, 2.0, 3.0, 4.0)
    println(doubleToString(d))

    println(filter(l)(_ % 2 == 0))

    val l2 = List(10, 20, 30)
    println(zip(l, l2))
    println(zipWith(l, l2)(_ + _))
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
      case Nil        => Nil
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Cons(_, Nil)) =>
      l match {
        case Nil          => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t)   => Cons(h, init(t))
      }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((l, r) => Cons(r, l))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def appendR[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map2[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(e => if (f(e)) List(e) else Nil)

  def zip(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) | (_, Nil)          => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zip(t1, t2))
  }

  def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] =
    (l, r) match {
      case (Nil, _) | (_, Nil)          => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

}
