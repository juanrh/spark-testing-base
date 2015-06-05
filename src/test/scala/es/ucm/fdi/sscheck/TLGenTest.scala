package es.ucm.fdi.sscheck

import org.scalacheck.{Properties, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, exists, AnyOperators, collect}
import util.Buildables.buildableSeq
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks._
import org.scalatest.Inspectors.{forAll => testForAll}

/*
 * NOTE the use of the import alias org.scalatest.Inspectors.{forAll => testForAll} to
 * distinguish between ScalaTest forAll inspector used in matchers, and ScalaTest
 * forAll adapter for ScalaCheck 
 * */

object TLGenTest extends Properties("TLGen temporal logic generators properties") {
  property("""now() applied to a batch generator returns a dstream generator 
      with  exactly one batch, and doesn't introduce new elements""") =
    forAll ("xs" |: arbitrary[List[Int]]) { xs : List[Int] =>
      // note here we are dropping some elements
      val g = TLGen.now(Gen.someOf(xs))
      forAll ("dstream" |: g) { dstream : DStream[Int] =>
        dstream should have length (1)
        dstream(0).length should be <= (xs.length) 
        testForAll (dstream(0)) { xs should contain (_)}
        true
      }
    }
  
  property("""now() applied to a constant batch generator returns a dstream generator
      with exactly that batch as the only batch""")  = 
    forAll ("xs" |: arbitrary[List[Int]]) { xs : List[Int] =>
      // using a constant generator
      val g = TLGen.now(xs)
      forAll ("dstream" |: g) { dstream : DStream[Int] =>
        dstream should have length (1)
        dstream(0) should be (xs)
        true
      }
    }
  
   // property("nextB() generates ") 
 //   def nextB[A](bg : Gen[Batch[A]]) : Gen[DStream[A]] = {

}