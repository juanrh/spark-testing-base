package es.ucm.fdi.scalacheck

import org.scalacheck.{Properties, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, BooleanOperators}
import util.Buildables.buildableSeq

object UtilsGenTest extends Properties("UtilsGenTest test") {
  property("""containerOfNtoM is able to generate sequences of 
		  string with size between N and M strings for both N and M >= 0""")  = 
    forAll (Gen.choose(0, 10), Gen.choose(0, 10)) { (n : Int, m : Int) => 
      val g = UtilsGen.containerOfNtoM(n, m, arbitrary[String]) : Gen[Seq[String]]
      forAll (g) { ( xs : Seq[String]) =>   
       xs.length >= n && xs.length <= m 
      }
    }
  
  property("repN respects its lenght constraints") = 
    forAll (Gen.choose(0, 10), Gen.choose(0, 10)) { (n : Int, xsLen : Int) => 
      val g = UtilsGen.repN(n, Gen.listOfN(xsLen, Gen.alphaStr)) 
      forAll (g) { (xs : Seq[String])  =>
        xs.length == xsLen * n
      }
    }
  
    property("repNtoM respects its lenght constraints") = 
    forAll (Gen.choose(0, 10), Gen.choose(0, 10), Gen.choose(0, 10)) { (n : Int, m : Int, xsLen : Int) => 
      val g = UtilsGen.repNtoM(n, m, Gen.listOfN(xsLen, Gen.alphaStr)) 
      forAll (g) { (xs : Seq[String])  =>
        val xsLenObs = xs.length
        xsLenObs >= xsLen * n && xsLen * n <= xsLen * m 
      }
    }  
}