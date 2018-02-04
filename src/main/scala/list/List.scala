package list

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = List.foldLeft(ints, 0)(_ + _)

  def product(ints: List[Int]): Int = List.foldLeft(ints, 1)(_ * _)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def setHead[A](x: A, xs: List[A]): List[A] = {
    xs match {
      case Nil => Cons(x, Nil)
      case Cons(_, xs) => Cons(x, xs)
    }
  }

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case _ => l
    }
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def foldRightWithLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(List.reverse(as), z)((a, b) => f(b, a))
  }

  def length[A](as: List[A]): Int = List.foldLeft(as, 0)((a, _) => a + 1)

  def append[A](as: List[A], z: A): List[A] = {
    List.reverse(Cons(z, foldLeft(as, Nil: List[A])((l, h) => {
      Cons(h, l)
    })))
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((z, h) => {
      Cons(h, z)
    })
  }
}
