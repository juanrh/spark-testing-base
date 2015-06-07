package es.ucm.fdi.spark.scalatest

import com.holdenkarau.spark.testing.SharedSparkContext
import org.scalatest.FunSuite
import org.scalatest.Matchers

class SampleShouldBeOnRDD extends FunSuite 
  with Matchers
  with SharedSparkContext {
 
  test("'should not be' works ok with RDD.isCheckpointed") {
    val rdd = sc.parallelize(1 to 100, 3)
    rdd should not be 'checkpointed
  }
  
  test("'shouldBe' and 'should not be' work ok with RDD.empty") {
    val n = 100
    val nonEmptyRdd = sc.parallelize(1 to n, 2)
    nonEmptyRdd should not be 'empty
    nonEmptyRdd.count should be (n)
    
    val emptyRDD = nonEmptyRdd filter ( _ < 0)
    emptyRDD shouldBe 'empty
  }
}