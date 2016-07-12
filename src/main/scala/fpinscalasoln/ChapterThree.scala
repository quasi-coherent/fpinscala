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

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

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

    // Generalize `fold` and `prod` to a higher-order function...
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    // and use this to re-write `sum` and `prod`.
    def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

    def prod2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

    // Compute the length of a list using `foldRight`.
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

    // Our implementation of `foldRight` is not stack-safe.  Write a
    // function `foldLeft` that is tail-recursive.
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // Re-write `sum` and `prod` using `foldLeft`.
    def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def prod3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    // A function to reverse a list using `fold`.
    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((l, a) => Cons(a, l))

    // Write `foldRight` in terms of `foldLeft`.  Now `foldRight` is tail-recursive.
    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)((b, a) => f(a, b))

    // Implement `append` using a `foldLeft`.
    def append[A](as: List[A], a: A): List[A] =
      foldRight2(as, Cons(a, Nil))((x, xs) => Cons(x, xs))

    // A function to flatten a list of lists.
    def flatten[A](ls: List[List[A]]): List[A] = ???
  }
}
