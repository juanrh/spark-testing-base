package es.ucm.fdi.sscheck

import org.scalacheck.{Properties, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, exists, AnyOperators, collect}
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks._
import org.scalatest.Inspectors.{forAll => testForAll}
import Batch.seq2batch
import BatchGen._
import DStreamGen._
import Buildables.{buildableBatch, buildableDStreamFromBatch}
import DStreamMatchers._

/** Tests for the LTL inspired HO generators defined at BatchGen and DStreamGen 
 *  
 * WARNING: due to using nested forall, shrinking might generate wrong counterexamples, 
 * so the _ORIGINAL versions should be used in case of test failure 
 * 
 * Note most tests check completeness of the generators, but not correctness, i.e. that
 * all the generated data corresponds to some input data. We could do that but logically
 * it makes sense the interpreting the TL formulas as requiring something to happen, but
 * not requiring that something doesn't happen, has we don't have negation.  
 * */
object TLGenTest extends Properties("TLGen temporal logic generators properties") {
  /*
   * NOTE the use of the import alias org.scalatest.Inspectors.{forAll => testForAll} to
   * distinguish between ScalaTest forAll inspector used in matchers, and ScalaTest
   * forAll adapter for ScalaCheck 
   * */  
  // 
  // Tests for BatchGen LTL inspired HO generators
  // 
  property("BatchGen.now() applied to a batch generator returns a dstream generator " +  
      "with exactly one batch, and doesn't introduce new elements") =
    forAll ("batch" |: arbitrary[Batch[Int]]) { batch : Batch[Int] =>
      // note here we are dropping some elements
      val g = BatchGen.now(Gen.someOf(batch))
      forAll ("dstream" |: g) { dstream : DStream[Int] =>
        dstream should have length (1)
        dstream(0).length should be <= (batch.length) 
        testForAll (dstream(0)) { batch should contain (_)}
        true
      }
    }
  
  property("BatchGen.now() applied to a constant batch generator returns a dstream generator " +
      "with exactly that batch as the only batch")  = 
    forAll ("batch" |: arbitrary[Batch[Int]]) { batch : Batch[Int] =>
      // using a constant generator
      val g = BatchGen.now(batch : Batch[Int])
      forAll ("dstream" |: g) { dstream : DStream[Int] =>
        dstream should have length (1)
        dstream(0) should be (batch)
        true
      }
    }
  
  property("BatchGen.next() returns a dstream generator that has exactly two batches, " + 
       "the first one is emptyBatch, and the second one is its argument") = 
    forAll ("batch" |: arbitrary[Batch[Int]]) { batch : Batch[Int] =>
      // using a constant generator
      val g = BatchGen.next(batch : Batch[Int])
      forAll ("dstream" |: g) { dstream : DStream[Int] =>
        dstream should have length (2)
        dstream(0) should be (Batch.empty)
        dstream(1) should be (batch)
        true
      }
    }  
  
   property("BatchGen.laterN(n, bg) generates n batches and then bg" ) =
    forAll ("batch" |: arbitrary[Batch[Int]], "n" |: Gen.choose(-10, 30))  { 
      (batch : Batch[Int], n : Int) =>
        // using a constant generator
        val g = BatchGen.laterN(n, batch)
        forAll ("nextDStream" |: g) { nextDStream : DStream[Int] =>
          nextDStream should have length (math.max(0, n) + 1)
          testForAll (nextDStream.slice(0, n)) {_ should be (Batch.empty)}
          nextDStream(nextDStream.length-1) should be (batch)
          true  
        }
    }
  
  property("BatchGen.until is a strong until, i.e. the second generator always occurs, and " +
      "the first one occours before")  =
    forAll ("batch1" |: arbitrary[Batch[Int]], "batch2" |: arbitrary[Batch[Int]]) { 
      (batch1 : Batch[Int], batch2 : Batch[Int]) =>
        // using constant generators
        val g = BatchGen.until(batch1, batch2)
        forAll ("untilDStream" |: g) { untilDStream : DStream[Int] =>
          testForAll (untilDStream.slice(0, untilDStream.length-1)) { _ should be (batch1)}
          untilDStream(untilDStream.length-1) should be (batch2)
          true
        }
    }

  property("BatchGen.eventually eventually produces data from the " + 
      "argument batch generator") = 
    forAll ("batch" |: arbitrary[Batch[Int]]) { batch : Batch[Int] =>
      forAll ("eventuallyDStream" |: BatchGen.eventually(batch)) { eventuallyDStream : DStream[Int] =>
        eventuallyDStream(eventuallyDStream.length-1) should be (batch)
        true
      }
    }
  
  property("BatchGen.always always produces data from the argument batch generator") = 
    forAll ("batch" |: arbitrary[Batch[Int]]) { batch : Batch[Int] =>
      forAll ("alwaysDStream" |: BatchGen.always(batch)) { alwaysDStream : DStream[Int] =>
        testForAll (alwaysDStream.toList) {_ should be (batch)}
        true
      }
    }
  
  property("BatchGen.release is a weak relase, i.e either bg2 happens forever, " + 
      "or it happens until bg1 happens, including the  moment when bg1 happens")  =
    forAll ("batch1" |: arbitrary[Batch[Int]], "batch2" |: arbitrary[Batch[Int]]) { 
      (batch1 : Batch[Int], batch2 : Batch[Int]) =>
        // using constant generators
        val g = BatchGen.release(batch1, batch2)
        forAll ("releaseDStream" |: g) { releaseDStream : DStream[Int] =>
          testForAll (releaseDStream.slice(0, releaseDStream.length-2)) { _ should be (batch2)}
          releaseDStream(releaseDStream.length-1) should  
            (be (batch1 ++ batch2) or be (batch2))
          true
        }
    }
  
  // 
  // Tests for DStreamGen LTL inspired HO generators
  //
  // small DStream generator for costly tests
  val smallDsg =  DStreamGen.ofNtoM(0, 10, BatchGen.ofNtoM(0, 5, arbitrary[Int]))
  
  property("DStreamGen.next() returns a dstream generator that has exactly 1 + the " +
      "number of batches of its argument, the first one is emptyBatch, and the rest " + 
      "are the batches generated by its argument") = 
    forAll ("dstream" |: arbitrary[DStream[Int]])  { dstream : DStream[Int] =>
      // using a constant generator
      val g = DStreamGen.next(dstream)
      forAll ("nextDStream" |: g) { nextDStream : DStream[Int] =>
        nextDStream should have length (1 + dstream.length)
        nextDStream(0) should be (Batch.empty)
        nextDStream.slice(1, nextDStream.size) should be (dstream)
        true
      } 
    }
  
  property("DStreamGen.laterN(n, dsg) generates n batches and then dsg" ) =
    forAll ("dstream" |: arbitrary[DStream[Int]], "n" |: Gen.choose(-10, 30))  { 
      (dstream : DStream[Int], n : Int) =>
        // using a constant generator
        val g = DStreamGen.laterN(n, dstream)
        forAll ("nextDStream" |: g) { nextDStream : DStream[Int] =>
          nextDStream should have length (math.max(0, n) + dstream.length)
          testForAll (nextDStream.slice(0, n)) {_ should be (Batch.empty)}
          nextDStream.slice(n, nextDStream.length) should be (dstream.toList)
          true  
        }
    }
  
  property("DStreamGen.until is a strong until, i.e. the second generator always occurs, " +
      "and the first one occours before") = {
    // explicitly limiting generator sizes to avoid too slow tests
    forAll ("dstream1" |: smallDsg, "dstream2" |: smallDsg) { 
      (dstream1 : DStream[Int], dstream2 : DStream[Int]) =>
        // using constant generators
        val (dstream1Len, dstream2Len) = (dstream1.length, dstream2.length)
        val g = DStreamGen.until(dstream1, dstream2)
        forAll ("untilDStream" |: g) { untilDStream : DStream[Int] =>
          for {i <- 0 until untilDStream.length - dstream1Len - dstream2Len} {
            dstream1 should beSubsetOf (untilDStream.slice(i, i + dstream1Len))
          }
          val tail = untilDStream.slice(untilDStream.length - dstream2Len, untilDStream.length)
          dstream2 should beSubsetOf(tail)
          true 
        }
    }
  }
   
  property("DStreamGen.eventually eventually produces data from the argument generator") = 
    forAll ("dstream" |: arbitrary[DStream[Int]]) { dstream : DStream[Int] =>
      forAll ("eventuallyDStream" |: DStreamGen.eventually(dstream)) { eventuallyDStream : DStream[Int] =>
        val eventuallyDStreamLen = eventuallyDStream.length
        val ending = eventuallyDStream.slice(eventuallyDStreamLen - dstream.length, eventuallyDStreamLen) 
        ending should be (dstream)
        true
      }
    }
  
  property("DStreamGen.always always produces data from the argument generator") =
    // explicitly limiting generator sizes to avoid too slow tests
    forAll (smallDsg) { dstream : DStream[Int] =>
      val dstreamLen = dstream.length
      forAll ("alwaysDStream" |: DStreamGen.always(dstream)) { alwaysDStream : DStream[Int] =>
        for {i <- 0 until alwaysDStream.length - dstreamLen} {
           dstream should beSubsetOf (alwaysDStream.slice(i, i + dstreamLen))
        }
        true
      }
    }
   
   property("DStreamGen.release is a weak relase, i.e either the second generator happens forever, " + 
      "or it happens until the first generator happens, including the  moment when the first generator happens")  =
     {
    // explicitly limiting generator sizes to avoid too slow tests
    forAll ("dstream1" |: smallDsg, "dstream2" |: smallDsg) { 
      (dstream1 : DStream[Int], dstream2 : DStream[Int]) =>
        // using constant generators
        val (dstream1Len, dstream2Len) = (dstream1.length, dstream2.length)
        val g = DStreamGen.release(dstream1, dstream2)
        forAll ("releaseDStream" |: g) { releaseDStream : DStream[Int] =>
          // this is similar to always, but note the use of max to account for
          // the case when dstream1 happens and dstream1 is longer than dstream2
          // We don't check if dstream1 happens, because it might not
          for {i <- 0 until releaseDStream.length - math.max(dstream1Len, dstream2Len)} {
           dstream2 should beSubsetOf (releaseDStream.slice(i, i + dstream2Len))
          }
          true 
        }
    }
  }
         
}