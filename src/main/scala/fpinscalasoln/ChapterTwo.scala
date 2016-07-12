package fpinscalasoln

object ChapterTwo {

  // Calculate the nth Fibonacci number
  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, curr: Int): Int =
      if (n <= 1) curr
      else loop(n - 1, curr, prev + curr)
    loop(n, 0, 1)
  }

  // Whether a list of type `A` is sorted according to `ordered`
  def isSorted[A](as: List[A])(ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    loop(0)
  }

  // Convert a function `f` of two arguments into a function of
  // one argument that partially applies f
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b) // Do not need to specify the types of `a` and `b`
                      // because they are inferred from the return type.

  // Reverse this
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Compose two functions
  def compose[A, B, C](f: A => B, g: B => C): A => C =
    a => g(f(a))

}
