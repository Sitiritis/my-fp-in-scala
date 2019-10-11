object Curry {
  def curry[T1, T2, T3](f: (T1, T2) => T3): T1 => (T2 => T3) =
    t1 => t2 => f(t1, t2)

  def uncurry[T1, T2, T3](f: T1 => (T2 => T3)): (T1, T2) => T3 =
    (t1, t2) => f(t1)(t2)
}
