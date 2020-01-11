package object RNG {
  type Rand[+A] = RNG => (A, RNG)
}
