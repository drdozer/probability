package uk.co.turingatemyhamster.prob.ga

import uk.co.turingatemyhamster.prob.{Utils, Generator}
import Generator._
/**
 * Created by nmrp3 on 14/12/13.
 */
object Generation {

  /**
   * A generator over the next population given the previous.
   *
   * @tparam T  the type of the individuals
   */
  type Breed[T] = Seq[T] => Generator[Seq[T]]

  /**
   * A generator over a population history, given a starting population.
   *
   * @tparam T  the type of the individuals
   */
  type PopulationHistory[T] = Seq[T] => Generator[Stream[Seq[T]]]

  /**
   * A generator over an individual given three parents.
   *
   * @tparam T  the type of the individuals
   */
  type MiniTournament[T] = (T, T, T) => Generator[T]

  /**
   * A generator over an individual given two parents.
   * 
   * @tparam T
   */
  type Breeder[T] = (T, T) => Generator[T]

  /**
   * A generator over an individual given a parent individual.
   * 
   * @tparam T
   */
  type Mutator[T] = T => Generator[T]

  /**
   * A generator over an ordering of individuals given individual scores.
   *
   * @tparam T
   */
  type RankFunction[T] = Seq[(T, Double)] => Generator[Seq[T]]

  def breed_1[T](b: Breeder[T]): Breeder[T] = { case (p, _) => p.identityG }
  def breed_2[T](b: Breeder[T]): Breeder[T] = { case (_, p) => p.identityG }

  def cloneOrCrossover[T](cloneP: Double, xoverP: Double)(mut: Mutator[T], xover: Breeder[T], chooser: Breeder[T]): Breeder[T] = {
    val clone = (p1: T, p2: T) => for(c <- chooser(p1, p2);
                    m <- mut(c)) yield m
    val options = Seq(clone -> cloneP, xover -> xoverP)

    (p1: T, p2: T) => for(cG <- options.oneWeighted;
                          c <- cG(p1, p2)) yield c
  }

  def populationHistory[T](b: Breed[T]): PopulationHistory[T] =
    (ts: Seq[T]) => Generator { rand =>
      val h = b(ts) generateFrom rand
      h #:: (populationHistory(b)(h) generateFrom rand)
  }

  def oneGeneration[T](sfGen: Generator[ScoreFunction[T]],
                       mut: Mutator[T],
                       xover: Breeder[T],
                       rankParents: RankFunction[T]): Breed[T] = (initialPopulation: Seq[T]) => {

    import Generator._
    import Utils._


    for(sf <- sfGen;
        chosen3 <- initialPopulation manyOf 3;
        ranked <- rankParents((chosen3.par map (i => i -> (sf score i))).seq);
        Seq(luckyParent, secondParent, victim) = ranked;
        childGen <- Seq(mut(luckyParent) -> 0.8,
          xover(luckyParent, secondParent) -> 0.2).oneWeighted;
        child <- childGen;
        nextGen = (initialPopulation removeFirst (_ == victim)) :+ child) yield
    {
      if(initialPopulation.size != nextGen.size) throw new IllegalStateException(f"The old population size was: ${initialPopulation.size} but the new population size is: ${nextGen.size}. Lost some!")
      nextGen
    }
  }

}
