object Fibonacci {
  def topDownFib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n == 0)
        prev
      else
        go(n - 1, cur, prev + cur)

    go(n, 0, 1)
  }

  def bottomUpFib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, i: Int, prev: Int, cur: Int): Int =
      if (i <= n)
        go(n, i + 1, cur, prev + cur)
      else
        prev

    go(n, 1, 0, 1)
  }
}
