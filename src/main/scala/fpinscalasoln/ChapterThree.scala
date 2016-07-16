package fpinscalasoln

object ChapterThree {

  /** In this chapter we implement the singly-linked list and binary tree data
    * structures in a functional way.
    *
    * Main concepts are pattern matching and folding.
    */

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
    def flatten[A](ls: List[List[A]]): List[A] = {
      // Helper function to concatenate two lists.
      def concat(l: List[A], r: List[A]): List[A] =
      foldRight2(l, r)(Cons(_, _))
      foldRight2(ls, Nil: List[A])(concat)
    }

    // As a preliminary to `map`, write a pure function to add one to each element
    // in a list of `Ints`.
    def addOne(ints: List[Int]): List[Int] =
    foldRight2(ints, Nil: List[Int])((int, l) => Cons(int + 1, l))

    // Implement `map`.
    def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

    // Implement `filter`.
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

    // Implement `flatMap`.
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      flatten(map(as)(f))

    // Use `flatMap` to implement `filter`.
    def filter2[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

    // Implement `zipWith`, which takes a `List[A]` and a `List[B]` and combines
    // corresponding elements of each using `f: (A, B) => C`.
    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    // Implement `hasSubsequence`, which determines whether a list contains another
    // list as a subsequence.
    def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
      // Helper function to determine if a list `as` starts with another list `sub`.
      def startsWith(as: List[A], sub: List[A]): Boolean = (as, sub) match {
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
        case _ => false
      }
      as match {
        case Nil => sub == Nil
        case _ if startsWith(as, sub) => true
        case Cons(h, t) => hasSubsequence(t, sub)
      }
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // A method to count the number of nodes in a `Tree`.  Not stack-safe.
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    // A function to determine the maximum element in a `Tree[Int]`.
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(int) => int
      case Branch(left, right) => maximum(left).max(maximum(right)) // Using built-in `max` on `Numeric`s.
    }

    // Write a function `depth` that returns the maximum path length from the root
    // to any leaf.
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(left, right) => depth(left).max(depth(right))
    }

    // Write a function `map` that modifies each element in a tree with a given function.
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    // Generalize these with `fold` that abstracts over their similarities.
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(a) => f(a)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

    // Rewrite `size`, `maximum`, `depth`, `map` with `fold`.
    def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

    def maximum2(t: Tree[Int]): Int = fold(t)(int => int)(_.max(_))

    def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _.max(_))

    def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }
}
