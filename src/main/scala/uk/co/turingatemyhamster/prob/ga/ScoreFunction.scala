package uk.co.turingatemyhamster.prob.ga

import scala.annotation.tailrec


trait ScoreFunction[T] {
  sf =>

  def score(t: T): Double

  def contramap[S](f: S => T): ScoreFunction[S] = new ScoreFunction[S] {
    def score(s: S) = sf score f(s)
  }

}

object ScoreFunction {

  def apply[T](f: T => Double):  ScoreFunction[T] = new ScoreFunction[T] { def score(t: T) = f(t) }

  def stringContainedWithin(s: String) = ScoreFunction { (s contains (_: String)) andThen booleanAsDouble }

  def stringNotContainedWithin(s: String) = ScoreFunction { (t: String) => booleanAsDouble(!(s contains t)) }

  def countOccurrencesWithin(s: String): String => Int =
    (t: String) => {
      @tailrec def count(pos:Int, c:Int):Int={
        val idx=s.indexOf(t, pos)
        if(idx == -1) c else count(idx+t.length, c+1)
      }
      count(0, 0)
    }

  def booleanAsDouble[T](b: Boolean): Double = if(b) 1.0 else 0.0

  def stringContainedWithinExactly(s: String, count: Int) = ScoreFunction {
    countOccurrencesWithin(s) andThen (_ == count) andThen booleanAsDouble }

  def stringContainedWithinExactlyOnce(s: String): ScoreFunction[String] = stringContainedWithinExactly(s, 1)

  implicit class ScoreFunctionSeqOps[T](val sfs: Seq[ScoreFunction[T]]) extends AnyVal {

    def reduceS(op: (Double, Double) => Double) = ScoreFunction { (t: T) => sfs map (_ score t) reduce op }
    def sumS = sfs reduceS (_ + _)
    def maxS = sfs reduceS Math.max
    def minS = sfs reduceS Math.min
  }

  def countMatching(seqs: Seq[String]) = (seqs map stringContainedWithin).sumS

  def countMatchingExactlyOnce(seqs: Seq[String]) = (seqs map stringContainedWithinExactlyOnce).sumS

  def countNonmatching(seqs: Seq[String]) = (seqs map stringNotContainedWithin).sumS

  implicit class ScoreFunctionOps[T](val sf: ScoreFunction[T]) extends AnyVal {

    // limited `map` restricted to Double.
    def andThen(f: Double => Double): ScoreFunction[T] = ScoreFunction { (t: T) => f(sf score t) }

    def * (tf: ScoreFunction[T]): ScoreFunction[T] = ScoreFunction { (t: T) => (sf score t) * (tf score t) }

    def * (s: Double): ScoreFunction[T] = sf andThen (_ * s)

    def / (s: Double): ScoreFunction[T] = sf andThen (_ / s)

    /** Map the score function from [0..1] to [-inf..+inf]. */
    def logit: ScoreFunction[T] = sf andThen ((x: Double) => Math.log(x) - Math.log(1.0 - x))

    /** Map the score function from [-inf..+inf] to [0..1]. */
    def logistic: ScoreFunction[T] = sf andThen ((x: Double) => 1.0 / (1.0 + Math.exp(-x)))

    /** Map the score function from [0..+inf] to [0..1]. This is just the top-half of `logistic`.*/
    def posLogistic: ScoreFunction[T] = logistic andThen (s => (s - 0.5) * 2.0)

    def inverse: ScoreFunction[T] = sf andThen (1.0 / _)

    def memoise: ScoreFunction[T] = {
      var memoMap: Map[T, Double] = Map()
      ScoreFunction { (t: T) => memoMap.synchronized { memoMap get t } match {
        case Some(s) => s
        case None =>
          val s = sf score t
          memoMap.synchronized { memoMap += (t -> s) }
          s
      }
      }
    }
  }

}
