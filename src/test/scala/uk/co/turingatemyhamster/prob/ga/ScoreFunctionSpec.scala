import org.specs2.mutable._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import scala.util.Random
import org.specs2.ScalaCheck

import uk.co.turingatemyhamster.prob._
import uk.co.turingatemyhamster.prob.ga.ScoreFunction

/**
 * Created with IntelliJ IDEA.
 * User: nmrp3
 * Date: 02/05/13
 * Time: 21:46
 * To change this template use File | Settings | File Templates.
 */
class ScoreFunctionSpec extends Specification with ScalaCheck {

  import ScoreFunction._

  "The stringContainedWithin score function" should {
    "score 1.0 for a string against itself" in {
      val str = "hi mum!"
      val scw = ScoreFunction.stringContainedWithin(str)
      scw score str must_== 1.0
    }

    "score 1.0 for a substring against a superstring" in {
      val sup = "hi mum!"
      val sub = "i mu"
      val scw = ScoreFunction.stringContainedWithin(sup)
      scw score sub must_== 1.0
    }

    "score 0.0 for a superstring against a substring" in {
      val sup = "hi mum!"
      val sub = "i mu"
      val scw = ScoreFunction.stringContainedWithin(sub)
      scw score sup must_== 0.0
    }
  }

  "The stringNotContainedWithin score function" should {
    "score 0.0 for a string against itself" in {
      val str = "hi mum!"
      val scw = ScoreFunction.stringNotContainedWithin(str)
      scw score str must_== 0.0
    }

    "score 0.0 for a substring against a superstring" in {
      val sup = "hi mum!"
      val sub = "i mu"
      val scw = ScoreFunction.stringNotContainedWithin(sup)
      scw score sub must_== 0.0
    }

    "score 1.0 for a superstring against a substring" in {
      val sup = "hi mum!"
      val sub = "i mu"
      val scw = ScoreFunction.stringNotContainedWithin(sub)
      scw score sup must_== 1.0
    }
  }

  "The stringContainedWithinExactlyOnce score function" should {
    "should score 1.0 for a string against itself" in {
      val seq = "attgccaccag"
      val cm = ScoreFunction.stringContainedWithinExactlyOnce(seq)
      cm score seq must_== 1.0
    }

    "should score 0.0 for a string against two coppies of itself" in {
      val seq = "attgccaccag"
      val cm = ScoreFunction.stringContainedWithinExactlyOnce(seq + seq)
      cm score seq must_== 0.0
    }
  }

  "The countMatching score function" should {

    "find 1 match against itself" in {
      val seq = "attgccaccag"
      val cm = ScoreFunction.countMatching(Seq(seq))
      cm score seq must_== 1.0
    }

    "find 3 match against 3 of itself" in {
      val seq = "attgccaccag"
      val cm = ScoreFunction.countMatching(Seq(seq, seq, seq))
      cm score seq must_== 3.0
    }

    "matches against a substring of itself" in {
      val rand = new Random(42)

      val bigSeq = "atcgtagtcgcgcgataatctagcgtagttaccg"
      val smallSeq = Generator.String.subString(bigSeq, 10) generateFrom rand

      val cm = ScoreFunction.countMatching(Seq(bigSeq))
      cm score smallSeq must_== 1.0
    }

  }

  "The countMismatching score function" should {
    "score 0.0 matches against itself" in {
      val seq = "attgccaccag"
      val cm = ScoreFunction.countNonmatching(Seq(seq))
      cm score seq must_== 0.0
    }

    "score 0.0 against 3 of itself" in {
      val seq = "attgccaccag"
      val cm = ScoreFunction.countNonmatching(Seq(seq, seq, seq))
      cm score seq must_== 0.0
    }

    "score 0.0 against a substring of itself" in {
      val rand = new Random(42)

      val bigSeq = "atcgtagtcgcgcgataatctagcgtagttaccg"
      val smallSeq = Generator.String.subString(bigSeq, 10) generateFrom rand

      val cm = ScoreFunction.countNonmatching(Seq(bigSeq))
      cm score smallSeq must_== 0.0
    }
  }
}
