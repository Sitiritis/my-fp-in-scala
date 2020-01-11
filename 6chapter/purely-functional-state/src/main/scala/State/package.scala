package object State {
  case class State[S, +A](run: S => (A, S)) {
    import State._

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, ns) = run(s)
      f(a).run(ns)
    })

    def map[B](f: A => B): State[S, B] =
      flatMap(unit[S, B] compose f)
  }

  object State {
    def unit[S, A]: A => State[S, A] = (a: A) => State((a, _))

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for { a <- sa; b <- sb } yield f(a, b)

    def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
      ls.foldRight(unit[S, List[A]](List.empty)){ (sa, sl) => map2(sa, sl){ _ :: _ } }

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for { s <- get; _ <- set(f(s)) } yield ()
  }
}
