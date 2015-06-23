package es.ucm.fdi.sscheck.prop

import org.scalacheck.{Properties, Test}

/** Convenient class that to define Scalacheck Properties objects 
 *  that can be easily run with custom Test.Parameters
 *  
 *  Example:
 *
{{{
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
}}}
 * */
class ConfiguredProperties(name : String) extends Properties(name) {
  /** Override this method in the subclass to customize the Test.Parameters 
   *  object that will be used to run this class when main() is called. 
   *  
   *  @param defaultParams value of Test.Parameters.default
   *  @return the default implementation returns Test.Parameters.default, subclasses
   *  should use methods of Test.Parameters like withMinSuccessfulTests() to modify
   *  the test execution parameters
   * */
  def testParameters(defaultParams : Test.Parameters) : Test.Parameters = defaultParams
  
  override def main(args: Array[String]): Unit = {  
    /* In the source for Prop.check and Properties.check we can see that this call Test.checkProperties
     * with this, and also with scalacheck.util.ConsoleReporter chained with whatever
     * Test.TestCallback is configured in this.testParameters
     * */
    super.check(testParameters _)
  }
}