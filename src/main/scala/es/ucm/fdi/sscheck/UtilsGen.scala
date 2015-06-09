package es.ucm.fdi.sscheck

import org.scalacheck.Gen
import org.scalacheck.util.Buildable
import Buildables.buildableSeq

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
  
  def buildableOfNtoM[C, T]
    (n : Int, m : Int, g : Gen[T])
	(implicit evb: Buildable[T, C], evt: (C) => Traversable[T])
	: Gen[C] = {
	  for {
        i <- Gen.choose(n, m)
        xs <- Gen.buildableOfN[C, T](i, g)
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
    
  /** Returns the generator that results from concatenating the sequences
   *  generated by g1 and g2 
   * */
  def concSeq[A](g1 : Gen[Seq[A]], g2 : Gen[Seq[A]]) : Gen[Seq[A]] = {
     for {
       xs <- g1
       ys <- g2
     } yield xs ++ ys
  } 
}