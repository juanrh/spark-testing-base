package es.ucm.fdi.sscheck.prop

import org.scalacheck.Prop.{forAll, AnyOperators}
import org.scalacheck.{Properties, Gen}
import org.scalacheck.Test
import org.scalatest._
import org.scalatest.Matchers._
import com.holdenkarau.spark.testing.StreamingSuiteBase
import es.ucm.fdi.sscheck.gen.{Batch, BatchGen, DStream => TDStream}
import org.apache.spark.streaming.dstream.DStream

// adding with StreamingSuiteBase gives an inheritance error, investigate 
object SSPropTest // extends Properties("example prop") {
                   extends ConfiguredProperties("example prop") {
  // TODO: create subclass ConfiguredProperties that use a implicit
  // based method similar to the ScalaTest way for this, test with simple props
  
  // overriding Properties.main to control test execution parameters
  override def testParameters(defaultParams : Test.Parameters) = defaultParams.withMinSuccessfulTests(5)
  
  // See TLGenIdeas5 for more on this
  val freqs = Map(1 -> 0, 4 -> 1)
  val batchSize = 5              
  val gBatchFreq = BatchGen.ofN(batchSize, Gen.frequency(freqs.mapValues(Gen.const(_)).toSeq:_*))
  val inputDsGen = Gen.resize(10, BatchGen.always(gBatchFreq))

  val expectedMean = {
    val freqS = freqs.toSeq
    val num = freqS .map({case (f, v) => v * f}). sum
    val den = freqS .map({case (f, _) => f}). sum
    num / den.toDouble
  }
  
  val expectedOutDsGen = Gen.resize(10, BatchGen.always(Batch(expectedMean)))  
  
  property("test mean") = forAll ("inputDStream" |: inputDsGen) { inputDstream : TDStream[Int] =>
     inputDstream.length should be >= (0)
     
     val input : Seq[Seq[Int]] = inputDstream
     
     // val input = List(List("hi"), List("hi holden"), List("bye"))
     // val expected = List(List("hi"), List("hi", "holden"), List("bye"))
     // testOperation[String, String](input, tokenize _, expected, useSet = true)
     
     true
   }
  // HERE: import trick has worked by using a different package, but
  // it is TODO to move all the files in package es.ucm.fdi.sscheck.gen
  // both in test and main to the corresponding new folder
  def meanSubject(input : DStream[Int]) : DStream[Double] =
    input.transform(rdd => rdd.sparkContext.parallelize(List(rdd.mean)))
  
  /*
   * NOTE: all the test cases should share the same spark streaming context, 
   * but that could be a problem to use the manual clock. So: :
   * - I don't know how could we stop one dstream without stopping the others, 
   * one option is using many dstreams in the same context and eval in parallel. 
   * It is not clear how to do that, try using futures for this concurrence
   * - we could use a pool of contexts with commons pool if needed to limit the 
   * parallelism at a single contex, but not clear if that is worth
   * */
  //FIXME  val runner = new StreamingSuiteBase {}  
  //FIXME  runner.beforeAll // this is needed to initialize the spark context, add some
                   // kind of setup phase to properties?, see clever tricks in book "testing spark"
  
  property("test avg alt") = 
    forAll ("inputDStream" |: inputDsGen, "expectedDStream" |: expectedOutDsGen) { 
      (inputDstream : TDStream[Int], expectedDstream : TDStream[Double]) =>
        // TODO: parallelize inputDstream, apply  meanSubject, and then collect
        // val observedDStream : DStream[Double]= ??? 
        // TODO: compare batch-wise expectedDstream and observedDStream with
        // the matcher with tolerance => zip and so
        
        // also try with withOutputAndStreamingContext with a matcher
        // that zips and asserts per batch
        
        /*runner.testOperation[Int, Double](inputDstream, 
        								 meanSubject _, 
        								 expectedDstream, useSet = true)
        								 * 
        								 */
        /*
         * This works but creates a new SparkStreamingContext per test case, see message 
         * 
         * 15/06/20 16:50:55 INFO SampleStreamingTest: Output generated in 405 milliseconds
         * 
         *  so:
         * 1. limit the number of test for fast development: it is possible that in general not
         * so much test cases can be generated
         * 2. try sharing the same SparkStreamingContext among several tests: 
         *   - create one DStream per test case
         *   - if 100 DStreams is too much, we could try to multiplex a pair DStream to represent
         *   several DStreams, but it is not clear how DStream transformation could be applied
         *   there
         *   - other option is using several SparkStreamingContext with a limit in the number of
         *   DStreams per context, to avoid scheduler paralysis. Note the all the contexts would be 
         *   executed sequentially. Note the parallelism of Spark, even if all the DStreams are in 
         *   a single context, is limited by the number of workers and controlled by the scheduler, 
         *   so a single context is the best. Its a pity that in Spark Streaming all the DStreams 
         *   are executed at the same time, vs in Spark batch where RDDs are more independent, this 
         *   is because DStreams are synchronous. Let's see what works better 
         * */
       // FIXME runner.testOperation[Int, Int](inputDstream, identity[DStream[Int]] _,
       // FIXME                                 inputDstream, useSet = false)  
        true  
  }  
}