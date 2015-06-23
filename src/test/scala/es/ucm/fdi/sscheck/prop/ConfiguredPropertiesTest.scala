package es.ucm.fdi.sscheck.prop

import org.scalacheck.Prop.{forAll, AnyOperators}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Test

object ConfiguredPropertiesTest extends ConfiguredProperties("Testing ConfiguredProperties") {
  /* overriding Properties.main to control test execution parameters
   * 
   * Now the all the tests should be executed 200 times, although that is not automatically checked 
   */
  override def testParameters(defaultParams : Test.Parameters) = defaultParams.withMinSuccessfulTests(200)
  
  property("commutativity of addition") = 
    forAll ("x" |: arbitrary[Int], "y" |: arbitrary[Int]) { (x : Int, y : Int) =>
      y + x ?= x + y 
    }
}