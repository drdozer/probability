package uk.co.turingatemyhamster.prob

import scala.util.Random
import Utils._

/**
 * A monadic abstraction for something which encapsulates the generation of instances of some type.
 */
trait Generator[T] {
  gen =>

  def generateFrom(rand: Random): T

  def map[U](f: T => U): Generator[U] = Generator { rand => f(gen generateFrom rand) }

  def flatMap[U](f: T => Generator[U]): Generator[U] = Generator { rand => f(gen generateFrom rand) generateFrom rand }
}

trait ConstructorCompanion[X, C] {
  def apply(c: C): X
  def unapply(x: X): Option[C]
}

object Generator {

  def apply[T](g: Random => T): Generator[T] = new Generator[T] {
    def generateFrom(rand: Random) = g(rand)
  }

  def identityG[T]: T => Generator[T] = (t: T) => Generator[T] { rand => t }

  def if_[A](g: Generator[Boolean])(onTrue: => A) = new {
    def else_(onFalse: => A) = g map (b => if(b) onTrue else onFalse)
  }

  implicit class ValueSyntax[V](val v: V) extends AnyVal {
    def |> [VV, W](f: VV => W)(implicit vv: V => VV): W = f(v)
    def dup: (V, V) = (v, v)
  }

  implicit class PairValueSyntax[V, W](val vw: (V, W)) extends AnyVal {
    def map2[A, B](va: V => A, wb: W => B) = (va(vw._1), wb(vw._2))
  }

  implicit class PairValueSyntaxG[V, W](val vw: (Generator[V], Generator[W])) extends AnyVal {
    def |@| : Generator[(V, W)] = Generator { rand => (vw._1 generateFrom rand, vw._2 generateFrom rand) }
  }

  def choose[T](gs: Seq[(Generator[T], Double)]): Generator[T] = Generator { rand =>
    gs.oneWeighted generateFrom rand generateFrom rand
  }

  def repeatN[T](n: Int, g: Generator[T]): Generator[Seq[T]] = Generator { rand =>
    Vector.fill(n) { g generateFrom rand } }

  implicit def stringFromChars(g: Generator[Seq[Char]]): Generator[String] = Generator { rand =>
    g generateFrom rand mkString "" }

  object Int {

    def nonNegative(i: Int): Generator[Int] = Generator[Int] { rand => rand nextInt i }

  }

  object String {
    // try to lift into a pure Generator[T]
    def subString(str: String, length: Int): Generator[String] = Generator { rand =>
      val i = rand.nextInt(str.length - length)
      str.substring(i, i + length)
    }
  }

  object Random {
    val nextBoolean = Generator[Boolean] { _.nextBoolean() }
    val nextDouble = Generator[Double] { _.nextDouble() }
    val nextFloat = Generator[Float] { _.nextFloat() }
    val nextGausian = Generator[Double] { _.nextGaussian() }
    val nextInt = Generator[Int] { _.nextInt() }
    def nextInt(i: Int) = Generator[Int] { _.nextInt(i) }
    val nextLong = Generator[Long] { _.nextLong() }
    val nextPrintableChar = Generator[Char] { _.nextPrintableChar() }
    def nextString(length: Int) = Generator[String] { _.nextString(length) }
  }

  implicit class NumericalGeneratorSyntax[T](val g: Generator[T])(implicit nn: Numeric[T]) {

    // numeric ops
    import nn.mkNumericOps
    def + (rhs: T) = g map (_ + rhs)
    def - (rhs: T) = g map (_ - rhs)
    def * (rhs: T) = g map (_ * rhs)
    def unary_-() = g map (_.unary_-())
    val abs = g map (_.abs())
    val signum = g map (_.signum())
    val toInt = g map (_.toInt())
    val toLong = g map (_.toLong())
    val toFloat = g map (_.toFloat())
    val toDouble = g map (_.toDouble())

    // comparison ops
    import nn.mkOrderingOps
    def < (rhs: T) = g map (_ < rhs)
    def <= (rhs: T) = g map (_ <= rhs)
    def > (rhs: T) = g map (_ > rhs)
    def >= (rhs: T) = g map (_ >= rhs)
    def equiv (rhs: T) = g map (_ equiv rhs)
    def max (rhs: T) = g map (_ max rhs)
    def min (rhs: T) = g map (_ min rhs)
  }

  implicit class GeneratorSyntax[T](val g: Generator[T]) extends AnyVal {
    def repeat(n: Int) = repeatN(n, g)
    def log(m: T => String) = g map (t => {
      println(m(t))
      t
    })

    def <> [S](cc: ConstructorCompanion[S, T]): Generator[S] = g <> (cc.apply, (s: S) => (cc unapply s).get)

    def <> [S](wrap: T => S, unwrap: S => T): Generator[S] = g map wrap

    def | (p: T => Boolean) = Generator { rand =>
      def rec: T = {
        val t = g generateFrom rand
        if(p(t)) t
        else rec
      }

      rec
    }

    def const[S] = (s: S) => g
  }

  implicit class SeqGeneratorSyntax[T](val seq: Seq[T]) extends AnyVal {
    def oneOf = {
      Generator { rand => (rand nextInt seq.length) |> seq }
    }

    def manyOf(n: Int): Generator[Seq[T]] = {
      if(n > seq.size) throw new IllegalArgumentException(f"Can't choose $n items from a seq with only ${seq.size} elements")
      else n match {
        case 0 => identityG(Seq())
        case _ => for(
          t <- seq.oneOf;
          seqNotT = seq removeFirst (_ == t);
          _ = if(seqNotT.size != seq.size - 1) throw new IllegalStateException(f"Supposed to remove one element from ${seq.size} items but got down to ${seqNotT.size} elements");
          chosen <- seqNotT manyOf (n-1)) yield chosen :+ t
      }
    }
  }

  implicit class SeqWithWeightSyntax[T](val seq: Seq[(T, Double)]) {

    def oneWeighted: Generator[T] = Generator {
      val s = seq.map(_._2).sum
      val ws = if(s == 0.0) seq map { case (t, w) => (t, 1.0 / seq.size.toDouble) }
        else seq map { case (t, w) => (t, w / s) }

      // try to lift into a pure Generator[T]
      rand => {
        def sel(is: Seq[(T, Double)], r: Double): T = {
          val (h, w) = is.head
          if(r <= w) h
          else sel(is.tail, r - w)
        }

        sel(ws, rand.nextDouble())
      }
    }

    def manyWeighted(n: Int): Generator[Seq[T]] =
      if(n > seq.size) throw new IllegalArgumentException(f"Can't choose $n items from a sequence with ${seq.size} items")
      else {
        n match {
          case 0 => identityG(Seq())
          case _ => for(
            t <- seq.oneWeighted;
            seqNotT = seq removeFirst {tw: (T, Double) => tw._1 == t};
            _ = if(seqNotT.size !=  seq.size - 1) throw new IllegalStateException(f"Supposed to remove one element from ${seq.size} items but got down to ${seqNotT.size} elements");
            chosen <- seqNotT manyWeighted (n-1)) yield chosen :+ t
        }
      }
  }
}

