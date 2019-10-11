object Composition {
  def compose(f: B => C, g: A => B): A => C =
    a => f(g(a))
}
