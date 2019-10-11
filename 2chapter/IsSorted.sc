object IsSorted {
  def isSorted[T](a: Array[T], lessThan: (T, T) => Boolean): Boolean = {
      @annotation.tailrec
      def go(n: Int): Boolean =
        if (n <= 0)
          true
        else if (lessThan(a(n), a(n - 1)))
          false
        else
          go(n - 1)

      go(a.size - 1)
  }
}
