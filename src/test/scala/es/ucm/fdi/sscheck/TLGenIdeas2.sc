package es.ucm.fdi.sscheck

import org.scalacheck.Gen
import org.scalacheck.Gen.sized
import org.scalacheck.Arbitrary.arbitrary
import util.Buildables.buildableSeq
import TLGen._

object TLGenIdeas2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val zeroBatch : Gen[Batch[Int]] = List(0)       //> zeroBatch  : org.scalacheck.Gen[es.ucm.fdi.sscheck.Batch[Int]] = org.scalach
                                                  //| eck.Gen$$anon$2@2b1de6a0
  nextB(zeroBatch). sample                        //> res0: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(), List(0)))
                                                  //| 
  now(List()).sample                              //> res1: Option[es.ucm.fdi.sscheck.DStream[Nothing]] = Some(List(List()))
  
  
/*  def always[A](bg : Gen[Batch[A]]) : Gen[DStream[A]] =
    Gen.containerOf[Seq, A](
    bg
    ) */
    
   val x : Gen[Batch[Int]] = Gen.const(List(0))   //> x  : org.scalacheck.Gen[es.ucm.fdi.sscheck.Batch[Int]] = org.scalacheck.Gen$
                                                  //| $anon$2@6e5170a8
 //  val a = Gen.containerOf(gb11)
  /*  sized { size =>
      
      ???
    } */
  
  def untilB[A](bg1 : Gen[Batch[A]], bg2 : Gen[Batch[A]]) : Gen[DStream[A]] =
    sized { size =>
      for {
        i <- Gen.choose(0, size - 1)
        // ok as Gen.listOfN(n, g) works as Gen.listOfN(0, g) for n < 0
        ds1 <- Gen.listOfN(i-1, bg1)
        ds2 <- now(bg2)
      } yield ds1 ++ ds2
  }                                               //> untilB: [A](bg1: org.scalacheck.Gen[es.ucm.fdi.sscheck.Batch[A]], bg2: org.s
                                                  //| calacheck.Gen[es.ucm.fdi.sscheck.Batch[A]])org.scalacheck.Gen[es.ucm.fdi.ssc
                                                  //| heck.DStream[A]]
   /*
   untilB should be defined from until ... or no untilB and use other combiners
   in until:
     - bg1 might happen after gb2 happens, now it's an exclusive => version then?
     - gb2 could happen not just once
   	 - this is a weak until? I don't think so but check, and add an automatic test for this
   */
  
  // FIXME
  def until[A](bg1 : Gen[Batch[A]], bg2 : Gen[Batch[A]]) : Gen[DStream[A]] =
    sized { size =>
      for {
        i <- Gen.choose(0, size - 1)
        // ok as Gen.listOfN(n, g) works as Gen.listOfN(0, g) for n < 0
        ds1 <- Gen.listOfN(i-1, bg1)
        ds2 <- now(bg2)
      } yield ds1 ++ ds2
      
    ???
  }                                               //> until: [A](bg1: org.scalacheck.Gen[es.ucm.fdi.sscheck.Batch[A]], bg2: org.s
                                                  //| calacheck.Gen[es.ucm.fdi.sscheck.Batch[A]])org.scalacheck.Gen[es.ucm.fdi.ss
                                                  //| check.DStream[A]]
  
  /* This implementation leads to an StackOverflow, as no lazyness is enforced,
  and the size is not controlled. Although it is based on a nice formula equivalence,
  a more direct approach based on the model for until seems simpler
  
  def until[A](bg1 : Gen[Batch[A]], bg2 : Gen[Batch[A]]) : Gen[DStream[A]] = {
    Gen.oneOf(
      now(bg2),
      untilAlt(bg1, bg2)
     )
  }
  
  def untilAlt[A](bg1 : Gen[Batch[A]], bg2 : Gen[Batch[A]]) : Gen[DStream[A]] = {
    concSeq(
      now(bg1),
      next(until(bg1, bg2))
    )
  }
 */
    
  def always[A](batchGen : Gen[Batch[A]]) : Gen[DStream[A]] = {
    ???
  }                                               //> always: [A](batchGen: org.scalacheck.Gen[es.ucm.fdi.sscheck.Batch[A]])org.s
                                                  //| calacheck.Gen[es.ucm.fdi.sscheck.DStream[A]]
  
 
  
  // TODO: overload like Gen.oneOf
  def concat[A](gs : Gen[Seq[A]]*) : Gen[Seq[A]] = {
    for {
      xs <- Gen.sequence(gs)
    } yield xs flatten
  }                                               //> concat: [A](gs: org.scalacheck.Gen[Seq[A]]*)org.scalacheck.Gen[Seq[A]]
  
  // val bg1 : Gen[Batch[Int]] = concat(Gen.oneOf(List(0), List(1)), Gen.oneOf(List(2), List(3)), Gen.oneOf(List(4), List(5)))
   // val bg1 : Gen[Batch[Int]] = concat(Gen.listOfN(1, Gen.oneOf(0, 1)), Gen.listOfN(1, Gen.oneOf(2, 3)))
   val bg1 : Gen[Batch[Int]] = concat(
     (for {
       i <- 0 to 4 by 2
    } yield Gen.listOfN(1, Gen.oneOf(i, i+1))  ):_*
   )                                              //> bg1  : org.scalacheck.Gen[es.ucm.fdi.sscheck.Batch[Int]] = org.scalacheck.G
                                                  //| en$$anon$6@3eddc72c
   
   bg1. sample                                    //> res2: Option[es.ucm.fdi.sscheck.Batch[Int]] = Some(List(1, 2, 5))
   bg1. sample                                    //> res3: Option[es.ucm.fdi.sscheck.Batch[Int]] = Some(List(0, 3, 4))

   val dsg1 : Gen[DStream[Int]] = Gen.oneOf(List(List(10)), List(List(11)))
                                                  //> dsg1  : org.scalacheck.Gen[es.ucm.fdi.sscheck.DStream[Int]] = org.scalachec
                                                  //| k.Gen$$anon$2@670fb8f3
   dsg1. sample                                   //> res4: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(10)))
   
   dstreamUnion(
     nextB(bg1),
     dsg1)
    .sample                                       //> res5: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(11), List(1,
                                                  //|  3, 4)))
   val dsg2 : Gen[DStream[Int]] = Gen.listOfN(3, List(-1))
                                                  //> dsg2  : org.scalacheck.Gen[es.ucm.fdi.sscheck.DStream[Int]] = org.scalachec
                                                  //| k.Gen$$anon$2@11412a45
   dsg2.sample                                    //> res6: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(-1), List(-1
                                                  //| ), List(-1)))
   untilB(dsg2, dsg1).sample                      //> res7: Option[es.ucm.fdi.sscheck.DStream[es.ucm.fdi.sscheck.Batch[Int]]] = S
                                                  //| ome(List(List(List(-1), List(-1), List(-1)), List(List(-1), List(-1), List(
                                                  //| -1)), List(List(-1), List(-1), List(-1)), List(List(-1), List(-1), List(-1)
                                                  //| ), List(List(-1), List(-1), List(-1)), List(List(-1), List(-1), List(-1)), 
                                                  //| List(List(-1), List(-1), List(-1)), List(List(-1), List(-1), List(-1)), Lis
                                                  //| t(List(-1), List(-1), List(-1)), List(List(-1), List(-1), List(-1)), List(L
                                                  //| ist(-1), List(-1), List(-1)), List(List(-1), List(-1), List(-1)), List(List
                                                  //| (-1), List(-1), List(-1)), List(List(-1), List(-1), List(-1)), List(List(-1
                                                  //| ), List(-1), List(-1)), List(List(-1), List(-1), List(-1)), List(List(-1), 
                                                  //| List(-1), List(-1)), List(List(-1), List(-1), List(-1)), List(List(-1), Lis
                                                  //| t(-1), List(-1)), List(List(-1), List(-1), List(-1)), List(List(-1), List(-
                                                  //| 1), List(-1)), List(List(-1), List(-1), List(-1)), List(List(-1), List(-1),
                                                  //|  List(-1)), List(List(-
                                                  //| Output exceeds cutoff limit.
   // note this uses the implicit Gen.const
   val nowZero : Gen[DStream[Int]] = now(List(0)) //> nowZero  : org.scalacheck.Gen[es.ucm.fdi.sscheck.DStream[Int]] = org.scalac
                                                  //| heck.Gen$$anon$2@65067a13
   nowZero . sample                               //> res8: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(0)))
  
   val onesUntilZero : Gen[DStream[Int]] = untilB(List(1, 1), List(0))
                                                  //> onesUntilZero  : org.scalacheck.Gen[es.ucm.fdi.sscheck.DStream[Int]] = org.
                                                  //| scalacheck.Gen$$anon$6@c38a936
   onesUntilZero. sample                          //> res9: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(1, 1), List(
                                                  //| 1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 
                                                  //| 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1),
                                                  //|  List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), Li
                                                  //| st(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(
                                                  //| 1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 
                                                  //| 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1),
                                                  //|  List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), Li
                                                  //| st(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(
                                                  //| 1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 
                                                  //| 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1),
                                                  //|  List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), List(1, 1), Li
                                                  //| st(1, 1), List(1, 1), L
                                                  //| Output exceeds cutoff limit.
   Gen.resize(15, onesUntilZero). sample          //> res10: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(1, 1), List
                                                  //| (1, 1), List(0)))
   val batchSize1 = 4                             //> batchSize1  : Int = 4
   val dstreamSize1 = 1                           //> dstreamSize1  : Int = 1
   def batchGen1 : Gen[Batch[Int]] = Gen.containerOfN(batchSize1, arbitrary[Int])
                                                  //> batchGen1: => org.scalacheck.Gen[es.ucm.fdi.sscheck.Batch[Int]]
   def dstreamGen1 : Gen[DStream[Int]] = Gen.containerOfN[Seq, Seq[Int]](dstreamSize1, batchGen1)
                                                  //> dstreamGen1: => org.scalacheck.Gen[es.ucm.fdi.sscheck.DStream[Int]]
   
   dstreamGen1.sample                             //> res11: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(-372325481,
                                                  //|  -2147483648, -2147483648, -1)))
   dstreamGen1.sample                             //> res12: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(0, 1, 21474
                                                  //| 83647, -1588602620)))
   dstreamUnion(dstreamGen1, dstreamGen1).sample  //> res13: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(2147483647,
                                                  //|  -1, -930133717, 0, 0, 1, -437268807, -2147483648)))
   dstreamUnion(dstreamGen1, dstreamGen1).sample  //> res14: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(2147483647,
                                                  //|  493430138, 2147483647, 2147483647, 0, -841902388, -1, 839389399)))
   dstreamUnion(List(List()), List(List(0))). sample
                                                  //> res15: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(0)))
   dstreamUnion(List(), List(List(0))). sample    //> res16: Option[es.ucm.fdi.sscheck.DStream[Int]] = Some(List(List(0)))
    dstreamUnion(List(List()), List()). sample    //> res17: Option[es.ucm.fdi.sscheck.DStream[Nothing]] = Some(List(List()))
   
   /*
   def dstreamUnion[A](gs1 : Gen[DStream[A]], gs2 : Gen[DStream[A]]) : Gen[DStream[A]] = {
    for {
      xs1 <- gs1
      xs2 <- gs2
    } yield xs1 zipAll(xs2, emptyBatch, emptyBatch) map(xs12 => xs12._1 ++ xs12._2)
  }
   */
   //now(0).sample
   /*
   
  - always : sized
  - concSeq como operador
   */
}