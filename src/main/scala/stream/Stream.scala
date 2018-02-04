package stream

import list._

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty
	
	def apply[A](as: A*): Stream[A] = 
		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    
    def toList[A](stream: => Stream[A]): List[A] = {
        stream match {
            case Cons(h, t) => list.Cons(h(), Stream.toList(t()))
            case _ => list.Nil
        }
    }

    def take[A](stream: => Stream[A], n: Int): Stream[A] = {
        stream match {
            case Cons(h, t) =>  
            case _ =>
        }
    }

}
