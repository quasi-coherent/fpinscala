package fpinscalasoln

/**
  * Created by dmdonohue on 7/7/16.
  */
object ChapterThree {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(l: List[Int]): Int = l match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }

    def product(l: List[Double]): Double = l match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(h, t) => h * product(t)
    }

    // Return the tail of the list.  We choose to return
    // the empty list if the tail of the empty list is
    // requested.
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    // Set the head of the list.  Again, we choose not to throw
    // an error if the empty list is passed to setHead.
    def setHead[A](h: A, l: List[A]): List[A] = l match {
      case Nil => Cons(h, Nil)
      case Cons(_, t) => Cons(h, t)
    }

    // Remove the first n elements of a list.
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
    }

    // Remove elements as long as `f` is true.
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  } // x: Int = 3
}
