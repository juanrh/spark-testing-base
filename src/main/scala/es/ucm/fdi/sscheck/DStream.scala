package es.ucm.fdi.sscheck

object DStream {    
  def empty[A] : DStream[A] = new DStream(List():_*)
    
  implicit def batchSeq2dstream[A](batches : Seq[Batch[A]]) : DStream[A] = DStream(batches:_*)
  implicit def seqSeq2dstream[A](batches : Seq[Seq[A]]) : DStream[A] = DStream(batches.map(Batch(_:_*)):_*)
}

/** Objects of this class represent discrete data streams  
 * */
case class DStream[A](batches : Batch[A]*) extends Seq[Batch[A]] {    
  override def toSeq : Seq[Batch[A]] = batches
  
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
}

