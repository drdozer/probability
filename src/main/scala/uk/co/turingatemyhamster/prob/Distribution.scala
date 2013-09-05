package uk.co.turingatemyhamster.prob


trait Distribution[Par, T] {
  def support(par: Par): Iterable[T]
  def pmf(par: Par, x: T): Double
  def cdf(par: Par, x: T): Double
  def median(par: Par): T = percentile(par, 0.5)
  def percentile(par: Par, pc: Double): T
  def mode(par: Par): T
  def variance(par: Par, x: T): Double
  def covariance(par: Par, x: T, y: T): Double

  // mgf, cf, pgf
}

//http://en.wikipedia.org/wiki/Categorical_distribution
trait CategoricalDistribution[Par, T] extends Distribution[Par, T] {
  def cdf(par: Par, x: T) = {
    def add(xs: Iterable[T], p: Double): Double = xs.headOption match {
      case None => p
      case Some(h) =>
        val pp = p + pmf(par, h)
        if(h == x) pp
        else add(xs.tail, p + pmf(par, h))
    }

    add(support(par), 0.0)
  }

  def percentile(par: Par, pc: Double) = {
    def m(xs: Iterable[T], p: Double): T = {
      val h = xs.head
      val pp = p + pmf(par, h)
      if(pp >= pc) h
      else m(xs.tail, pp)
    }

    m(support(par), 0.0)
  }

  def mode(par: Par) = {
    support(par).maxBy(pmf(par, _))
  }

  def variance(par: Par, x: T) = {
    val p = pmf(par, x)
    p * (1.0 - p)
  }

  def covariance(par: Par, x: T, y: T) = -pmf(par, x)*pmf(par, y)
}

object CategoricalDistribution {

  implicit def mapAsCD[T]: CategoricalDistribution[Map[T, Double], T] = new CategoricalDistribution[Map[T, Double], T] {
    def support(par: Map[T, Double]) = par.keys

    def pmf(par: Map[T, Double], x: T) = par get x getOrElse 0.0
  }

  implicit def seqAsCD[T]: CategoricalDistribution[Seq[(T, Double)], T] = new CategoricalDistribution[Seq[(T, Double)], T] {
    def support(par: Seq[(T, Double)]) = par map (_._1)

    def pmf(par: Seq[(T, Double)], x: T) = (par filter (_._1 == x) map (_._2)).headOption getOrElse 0.0
  }

  implicit def parArrayAsCD[T]: CategoricalDistribution[(Array[T], Array[Double]), T] = new CategoricalDistribution[(Array[T], Array[Double]), T] {
    def support(par: (Array[T], Array[Double])) = par._1

    def pmf(par: (Array[T], Array[Double]), x: T) = {
      val s = par._1
      val p = par._2
      val l = s.length

      def w(i: Int): Double = if(s(i) == x) p(i) else if(i < l) w(i+1) else 0.0

      w(0)
    }
  }

  implicit val parray: CategoricalDistribution[Array[Double], Int] = new CategoricalDistribution[Array[Double], Int] {
    def support(par: Array[Double]) = 0 until par.length

    def pmf(par: Array[Double], x: Int) = par(x)
  }

  implicit class CD[Par, T](val par: Par)(implicit cd: CategoricalDistribution[Par, T]) /*extends AnyVal*/ {
    def generator: Generator[T] = Generator { rand => cd.percentile(par, rand.nextDouble()) }
  }
}