package es.ucm.fdi.sscheck.gen

import scala.collection.immutable.{HashBag=>Bag}
import org.scalatest.matchers.{Matcher, MatchResult}

object DStream {    
  def empty[A] : DStream[A] = new DStream(List():_*)
    
  implicit def batchSeq2dstream[A](batches : Seq[Batch[A]]) : DStream[A] = DStream(batches:_*)
  implicit def seqSeq2dstream[A](batches : Seq[Seq[A]]) : DStream[A] = DStream(batches.map(Batch(_:_*)):_*)
}

/** Objects of this class represent discrete data streams  
 * */
case class DStream[A](batches : Batch[A]*) extends Seq[Batch[A]] {    
  override def toSeq : Seq[Batch[A]] = batches
  def toBagSeq : Seq[Bag[A]] = batches.map(_.toBag)
  
  override def apply(idx : Int) = batches.apply(idx)
  override def iterator = batches.iterator
  override def length = batches.length
  
  // Note def ++(other : DStream[A]) : DStream[A] is inherited from Seq[_]
    
  /** @return a DStream for the batch-by-batch concatenation of this
  *  and other. Note we fill either dstreams with empty batches. This 
  *  implies dstreams are implicitly treated as they where infinitely extended 
  *  with empty batches  
  */
  def #+(other : DStream[A]) : DStream[A] = {
    batches. zipAll(other, Batch.empty, Batch.empty)
           . map(xs12 => xs12._1 ++ xs12._2)
  }

  /** @return true iff each batch of this dstream is contained in the batch
   *  at the same position in other. Note this implies that true can be
   *  returned for cases when other has more batches than this
   * */
  def subsetOf(other : DStream[A]) : Boolean = {
    batches
      .zip(other.batches)
      .map({case (thisBatch, otherBatch) =>
              thisBatch.forall(otherBatch.contains(_))
           })
      .forall(identity[Boolean])
  } 
}

trait DStreamMatchers {
  class DStreamSubsetOf[A](expectedSuperDStream : DStream[A]) extends Matcher[DStream[A]] {
    override def apply(observedDStream : DStream[A]) : MatchResult = {      
      // FIXME reimplement with Inspector for better report
      MatchResult(observedDStream.subsetOf(expectedSuperDStream), 
      			s"""$observedDStream is not a pointwise subset of $expectedSuperDStream""", 
      			s"""$observedDStream is a pointwise subset of $expectedSuperDStream""")
    }
  }
  def beSubsetOf[A](expectedSuperDStream : DStream[A]) = new DStreamSubsetOf(expectedSuperDStream)
}
object DStreamMatchers extends DStreamMatchers
