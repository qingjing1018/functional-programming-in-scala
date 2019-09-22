/* Return the sum of elements between a and b */

def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

sum(x => x * x * x, 3, 5)

// Equivalently
def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)


