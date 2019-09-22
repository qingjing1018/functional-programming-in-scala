/*
object rationals {
  val x = new Rational(1, 2)
  x.numer
  x.denom


  class Rational(x: Int, y: Int) {
    def numer = x

    def denom = y
  }

  // We can use class Rational as a pure data type
    def addRational(r: Rational, s: Rational): Rational =
      new Rational(
        r.numer * s.denom + r.denom * s.numer,
        r.denom * s.denom
      )
    def makeString(r: Rational) =
      r.numer + "/" + r.denom
}

rationals.makeString(rationals.addRational(new rationals.Rational(1,2), new rationals.Rational(2,3)))
*/

// We can package functions operating on a data abstraction in the data abstraction itself (called methods)
object rational {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  class Rational(x: Int, y: Int) {
    require(y !=0, "denominator must not be 0")

    def this(x: Int) = this(x, 1)  // second construction

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)
    def numer = x / g

    def denom = y / g

    def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

    // 'this' refers to the current rational
    def max(that: Rational) =
      if (this.less(that)) that else this

    def add(that: Rational): Rational =
      new Rational(
        numer * that.denom + denom * that.numer,
        denom * that.denom)

    def neg: Rational =
      new Rational(-numer, denom)

    def sub(that: Rational): Rational = add(that.neg)

    override def toString = numer + "/" + denom
  }
}

rational.x
rational.y
rational.z

rational.x.sub(rational.y).sub(rational.z)
rational.y.add(rational.y)
rational.x.less(rational.y)
rational.x.max(rational.y)



