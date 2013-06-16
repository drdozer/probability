package mtgenomes


trait Distribution[Par[_]] {
  def support[T](par: Par[T]): Iterable[T]
  def pmf[T](par: Par[T], x: T): Double
  def cdf[T](par: Par[T], x: T): Double
  def median[T](par: Par[T]): T
  def mode[T](par: Par[T]): T
  def variance[T](par: Par[T], x: T): Double
  def covariance[T](par: Par[T], x: T, y: T): Double

  // mgf, cf, pgf
}

//http://en.wikipedia.org/wiki/Categorical_distribution
trait CategoricalDistribution[Par[_]] extends Distribution[Par] {
  def cdf[T](par: Par[T], x: T) = {
    def add(xs: Iterable[T], p: Double): Double = xs.headOption match {
      case None => p
      case Some(h) =>
        val pp = p + pmf(par, h)
        if(h == x) pp
        else add(xs.tail, p + pmf(par, h))
    }

    add(support(par), 0.0)
  }

  def median[T](par: Par[T]) = {
    def m(xs: Iterable[T], p: Double): T = {
      val h = xs.head
      val pp = p + pmf(par, h)
      if(pp >= 0.5) h
      else m(xs.tail, pp)
    }

    m(support(par), 0.0)
  }

  def mode[T](par: Par[T]) = {
    support(par).maxBy(pmf(par, _))
  }

  def variance[T](par: Par[T], x: T) = {
    val p = pmf(par, x)
    p * (1.0 - p)
  }

  def covariance[T](par: Par[T], x: T, y: T) = -pmf(par, x)*pmf(par, y)
}

object CategoricalDistribution {
  trait distribution extends Distribution[CategoricalDistribution] {
    // supply support & pmf
    def pdf[T](cd: CategoricalDistribution[T])
  }

  type MapK[K] = Map[K, Double]
  implicit val mapAsCD: CategoricalDistribution[MapK] = new CategoricalDistribution[MapK] {
    def support[T](par: CategoricalDistribution.MapK[T]) = par.keys

    def pmf[T](par: CategoricalDistribution.MapK[T], x: T) = par get x getOrElse 0.0
  }

  type SeqT[T] = Seq[(T, Double)]
  implicit val seqAsCD: CategoricalDistribution[SeqT] = new CategoricalDistribution[CategoricalDistribution.SeqT] {
    def support[T](par: CategoricalDistribution.SeqT[T]) = par map (_._1)

    def pmf[T](par: CategoricalDistribution.SeqT[T], x: T) = (par filter (_._1 == x) map (_._2)).headOption getOrElse 0.0
  }

  type ParArray[T] = (Array[T], Array[Double])
  implicit val parArrayAsCD: CategoricalDistribution[ParArray] = new CategoricalDistribution[CategoricalDistribution.ParArray] {
    def support[T](par: (Array[T], Array[Double])) = par._1

    def pmf[T](par: (Array[T], Array[Double]), x: T) = {
      val s = par._1
      val p = par._2
      val l = s.length

      def w(i: Int): Double = if(s(i) == x) p(i) else if(i < l) w(i+1) else 0.0

      w(0)
    }
  }
}