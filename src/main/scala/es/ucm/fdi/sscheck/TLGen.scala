package es.ucm.fdi.sscheck

import org.scalacheck.Gen
import org.scalacheck.Gen.sized
import util.Buildables.buildableSeq
import UtilsGen.{containerOfNtoM,concSeq}

object TLGen {  
  // implicit def batchGen2dstreamGen[A](bg : Gen[Batch[A]]) : Gen[DStream[A]] = now(bg)
  /** Note using this method to define an implicit from Gen[Batch[A]] to Gen[DStream[A]] 
   *  is a bad idea because the type system confuses one with the other, as for example
   *  List() has types both Batch[Nothing] and DStream[Nothing]. Another possibility is replacing
   *  the type alias by case classes, which implies more object involved but makes 
   *  the types distinguishable
   * */
  def now[A](bg : Gen[Batch[A]]) : Gen[DStream[A]] = Gen.listOfN(1, bg)
  
  def nextB[A](bg : Gen[Batch[A]]) : Gen[DStream[A]] = {
    next(now(bg))
  } 
  
  def next[A](dsg : Gen[DStream[A]]) : Gen[DStream[A]] = {
  	concSeq(
  	  now(emptyBatch), 
  	  dsg
  	)
  }
  
}