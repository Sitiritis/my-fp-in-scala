object Factorial {
  def bottomUpFactorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, accumulator: Int): Int =
      if (current <= n)
        go(n, current + 1, current * accumulator)
      else
        accumulator

    go(n, 1, 1)
  }

  def topDownFactorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, accumulator: Int): Int =
      if (n >= 1)
        go(n - 1, n * accumulator)
      else
        accumulator

    go(n, 1)
  }
}
