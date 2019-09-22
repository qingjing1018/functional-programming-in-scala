object exercise {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  // Represent factorial in terms of product
  def fact(n: Int): Int = product(x => x)(1, n)
}

exercise.product(x => x * x)(3, 4)

exercise.fact(5)

object exerciseGeneralized {
  // Generalize sum and product in one function
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

  def product(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x + y, 0)(a, b)

  def fact(n: Int): Int = product(x =>x)(1, n)
}

exerciseGeneralized.fact(6)