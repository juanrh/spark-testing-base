package es.ucm.fdi.sscheck

import org.scalacheck.Gen
import util.Buildables.buildableSeq
import UtilsGen.{containerOfNtoM}
import ReGen._
import UtilsGen._

/**
TODO: consider scalaz-streams for the generator, or at least inspire with it,
see time.awakeEvery. See chapter 15 "Stream processing and incremental IO" in
"Functional Programming in Scala". Anyway, it seems this project is almost dead,
ony two developers and only maintenance, and this is an interpreted DSL so its
efficiency less efficient than akka streams
*/

object TLGenIdeas {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val zeroStream : Gen[Seq[Seq[Double]]] = containerOfNtoM(2, 5, repNtoM(1, 3, alt(symbol(0), symbol(0.01), symbol(-0.01))))
                                                  //> zeroStream  : org.scalacheck.Gen[Seq[Seq[Double]]] = org.scalacheck.Gen$$ano
                                                  //| n$6@1b8acf07
  zeroStream.sample                               //> res0: Option[Seq[Seq[Double]]] = Some(List(List(0.01, 0.0), List(-0.01), Lis
                                                  //| t(0.0, -0.01, -0.01), List(0.0, -0.01, 0.01)))
  zeroStream.sample                               //> res1: Option[Seq[Seq[Double]]] = Some(List(List(0.0, 0.0, 0.01), List(0.0, 0
                                                  //| .0), List(-0.01), List(0.01), List(-0.01, -0.01)))
  
  // TODO: working with List instead of Seq: could use Seq but List is more native to ScalaCheck and this way
  // it is more clear which is the performance we get
  // TODO: this might well be an operator, through a wrapper class + implicit conversion: respect the return type outside the wrapper
  /**
  * Analogous to Haskell's concat TODO doc, and move ReGen.conc and ReGen.epsilon to where this code goes
  */
  def concat[A](gs : Gen[Seq[A]]*) : Gen[Seq[A]] = gs.tail.foldLeft(gs(0))(ReGen.conc)
                                                  //> concat: [A](gs: org.scalacheck.Gen[Seq[A]]*)org.scalacheck.Gen[Seq[A]]
  val cs = concat(Gen.oneOf(List(0), List(1)), Gen.oneOf(List(2), List(3)), Gen.oneOf(List(4), List(5)))
                                                  //> cs  : org.scalacheck.Gen[Seq[Int]] = org.scalacheck.Gen$$anon$6@6ba6895c
  cs.sample                                       //> res2: Option[Seq[Int]] = Some(List(1, 3, 4))
  cs.sample                                       //> res3: Option[Seq[Int]] = Some(List(0, 2, 5))
  // TODO: sized version
  type Batch[A] = Seq[A]
  type DStream[A] = Seq[Batch[A]]
  def next[A](batchGen : Gen[Batch[A]]) : Gen[DStream[A]] = {
  	for { batch <- batchGen
  	      empty = List()
  	} yield List(empty, batch)
  }                                               //> next: [A](batchGen: org.scalacheck.Gen[es.ucm.fdi.sscheck.TLGenIdeas.Batch[
                                                  //| A]])org.scalacheck.Gen[es.ucm.fdi.sscheck.TLGenIdeas.DStream[A]]
  
  next(cs). sample                                //> res4: Option[es.ucm.fdi.sscheck.TLGenIdeas.DStream[Int]] = Some(List(List()
                                                  //| , List(1, 2, 5)))
  val cs2 = Gen.oneOf(List(List(10)), List(List(11)))
                                                  //> cs2  : org.scalacheck.Gen[List[List[Int]]] = org.scalacheck.Gen$$anon$2@6e6
                                                  //| 49f9f
  def streamGenUnion[A](g1 : Gen[DStream[A]], g2 : Gen[DStream[A]]) : Gen[DStream[A]] = {
    // TODO: this doesn't work, must ensure holes are added in either sequences if
    // they are not of the same size. Generalize to n sequences. Consider using Scala Stream class
    for {
      xs1 <- g1
      xs2 <- g2
    } yield xs1 zip(xs2) map(xs12 => xs12._1 ++ xs12._2)
  }                                               //> streamGenUnion: [A](g1: org.scalacheck.Gen[es.ucm.fdi.sscheck.TLGenIdeas.DS
                                                  //| tream[A]], g2: org.scalacheck.Gen[es.ucm.fdi.sscheck.TLGenIdeas.DStream[A]]
                                                  //| )org.scalacheck.Gen[es.ucm.fdi.sscheck.TLGenIdeas.DStream[A]]
  // HERE FIXME: this is NOT what we need, we need a pointwise union => zipWith concat
  streamGenUnion(next(cs), cs2).sample            //> res5: Option[es.ucm.fdi.sscheck.TLGenIdeas.DStream[Int]] = Some(List(List(1
                                                  //| 1)))
  streamGenUnion(next(cs), cs2).sample            //> res6: Option[es.ucm.fdi.sscheck.TLGenIdeas.DStream[Int]] = Some(List(List(1
                                                  //| 1)))
  streamGenUnion(next(cs), cs2).sample            //> res7: Option[es.ucm.fdi.sscheck.TLGenIdeas.DStream[Int]] = Some(List(List(1
                                                  //| 1)))
  streamGenUnion(next(cs), cs2).sample            //> res8: Option[es.ucm.fdi.sscheck.TLGenIdeas.DStream[Int]] = Some(List(List(1
                                                  //| 1)))
  // ReGen.conc(ReGen.epsilon, batchGen)
  /*
    val epsilon = Gen.const(List())
  
   def repNtoM[A](n : Int, m : Int, g : Gen[Seq[A]]) : Gen[Seq[A]] = {
  	for {
	  xs <- containerOfNtoM(n, m, g)
	} yield xs flatten
  }
  def alt[A](gs : Gen[Seq[A]]*) : Gen[Seq[A]] = {
    val l = gs.length
    // require (l > 0, "alt needs at least one alternative")
    if (l == 0)
       epsilon
  	else if (l == 1)
  		gs(0)
  	else
      Gen.oneOf(gs(0), gs(1), gs.slice(2, l):_*)
  }
  def containerOfNtoM[C[_], T]
	(n : Int, m : Int, g : Gen[T])
	(implicit evb: Buildable[T, C[T]], evt: (C[T]) => Traversable[T])
	: Gen[C[T]] = {
	  for {
        i <- Gen.choose(n, m)
        xs <- Gen.containerOfN[C, T](i, g)
      } yield xs
  }
  
  def star[A](g : Gen[Seq[A]]) : Gen[Seq[A]] = {
		for {
	    xs <- Gen.containerOf(g)
	  } yield xs flatten
  }
  
  def conc[A](g1 : Gen[Seq[A]], g2 : Gen[Seq[A]]) : Gen[Seq[A]] = {
     for {
       xs <- g1
       ys <- g2
     } yield xs ++ ys
  }
  */
  
}