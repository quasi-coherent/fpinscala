package fpinscalasoln

object ChapterThree {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(h, t) => h * product(t)
    }

    // Return the tail of the list.  We choose to return
    // the empty list if the tail of the empty list is
    // requested.
    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    // Set the head of the list.  Again, we choose not to throw
    // an error if the empty list is passed to setHead.
    def setHead[A](a: A, as: List[A]): List[A] = as match {
      case Nil => Cons(a, Nil)
      case Cons(_, t) => Cons(a, t)
    }

    // Remove the first n elements of a list.
    def drop[A](as: List[A], n: Int): List[A] = {
      if (n <= 0) as
      else as match {
        case Nil => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
    }

    // Remove elements as long as `f` is true.
    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => as
    }

    // Return all but the last element. Why can't this function
    // be implemented in constant time?
    def init[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

    def prod2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

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
