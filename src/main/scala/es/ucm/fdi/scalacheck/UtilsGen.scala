package es.ucm.fdi.scalacheck

import org.scalacheck.Gen
import org.scalacheck.util.Buildable
import util.Buildables.buildableSeq

object UtilsGen {
  /** Like containerOfN but with variable number of elements
   * */
  def containerOfNtoM[C[_], T]
	(n : Int, m : Int, g : Gen[T])
	(implicit evb: Buildable[T, C[T]], evt: (C[T]) => Traversable[T])
	: Gen[C[T]] = {
	  for {
        i <- Gen.choose(n, m)
        xs <- Gen.containerOfN[C, T](i, g)
      } yield xs
  }
  
  /** Generates n sequences from g and concatenates them
   *  */
  def repN[A](n : Int, g : Gen[Seq[A]]) : Gen[Seq[A]] = {
	for {
	  xs <- Gen.containerOfN(n, g)
	} yield xs flatten
  }     
  
  /** Generates i sequences from g, with i between n and m, and concatenates them
   *  */
  def repNtoM[A](n : Int, m : Int, g : Gen[Seq[A]]) : Gen[Seq[A]] = {
  	for {
	  xs <- containerOfNtoM(n, m, g)
	} yield xs flatten
  }         
}