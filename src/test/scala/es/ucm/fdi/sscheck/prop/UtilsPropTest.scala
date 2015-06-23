package es.ucm.fdi.sscheck.prop

import org.scalacheck.{Properties, Gen}
import org.scalacheck.Prop.{forAll, exists, AnyOperators}
import org.scalacheck.Prop
import org.scalatest._
import org.scalatest.Matchers._
import scala.util.{Try}
import UtilsProp.safeExists

/* This object contains several test that we want to fail, so its correct outcome is that 
 *  failure. Hence we want the test suite to succeed when those test fail. For that we 
 *  use equality comparisons to Prop.exception or Prop.falsified. 
 * */
object UtilsPropTest extends Properties("Breaking Prop.exists with ScalaTest matchers") {
  property("forAll works ok with ScalaTest matchers, for successful tests") = 
    forAll { x : Int => 
      x + x should be (2 * x)
      true
    }
 
  property("forAll works ok with ScalaTest matchers, for failing tests") =
    // Prop.throws only works inside the forAll, before the exception
    // has been propagated during the evaluation of the property for a 
    // generated value
    (forAll { x : Int => 
      x + x should be (2 * x + 1)
      true
    }) == Prop.exception 
  
  property("exists works ok without ScalaTest matchers") =
    exists ("x" |: Gen.choose(1, 10)) { x : Int =>
      x ?= 1
    }
  
  /* I have not been able to reify this successfully
  property("exists fails with ScalaTest matchers, for otherwise successful tests") =
   (exists ("x" |: Gen.choose(0, 10)) { x : Int =>
      x should be (5)
      true
    }) == Prop.exception  
  */
  
  property("exists works ok with ScalaTest matchers wrapped by Try") = 
    exists ("x" |: Gen.choose(0, 10)) { x : Int =>
      Try(x should be (1)).isSuccess 
    }

  property("safeExists works ok with ScalaTest matchers, for successful tests") =
    safeExists("x" |: Gen.choose(0, 10)) { x : Int =>
      x should be (1)
      true
    }
  
  property("safeExists works ok with ScalaTest matchers, for failing tests") =
    (safeExists("x" |: Gen.choose(0, 10)) { x : Int =>
      x should be (1000)
      true
    }) == Prop.undecided
}