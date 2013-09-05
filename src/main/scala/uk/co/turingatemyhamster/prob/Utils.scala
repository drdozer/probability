package uk.co.turingatemyhamster.prob

/**
 * Created by nmrp3 on 03/06/13.
 */
object Utils {

  implicit class SeqOps[T](val seq: Seq[T]) extends AnyVal {
    def removeFirst(p: T => Boolean): Seq[T] = {
      val (h, ts) = seq partition ((x: T) => !p(x))
      h ++ ts.tail
    }

  }

  def const[A, B](b: B): B = b
}
