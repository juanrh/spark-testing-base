package es.ucm.fdi

import org.scalacheck.Gen
import org.scalacheck.util.Buildable
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

package sscheck {
  // TODO: move some things outside this file, or merge other
  // files, but use a coherent code organization
  object Batch {
    def empty[A] : Batch[A] = new Batch(points = List())
    
    implicit def seq2batch[A](seq : Seq[A]) : Batch[A] = Batch(seq)
    
    implicit def batchGen2seqGen[A](g : Gen[Batch[A]]) : Gen[Seq[A]] = g.map(_.toSeq)
    implicit def seqGen2batchGen[A](g : Gen[Seq[A]]) : Gen[Batch[A]] = g.map(Batch(_))
  }
  case class Batch[A](points : Seq[A]) extends Seq[A] { 
    override def toSeq : Seq[A] = points
    
    override def apply(idx : Int) = points.apply(idx)
    override def iterator = points.iterator
    override def length = points.length
  }

  object DStream {    
    implicit def batchSeq2dstream[A](batches : Seq[Batch[A]]) : DStream[A] = DStream(batches)
    implicit def seqSeq2dstream[A](batches : Seq[Seq[A]]) : DStream[A] = DStream(batches.map(Batch(_)))
    
    implicit def dstreamGen2batchSeqGen[A](gs : Gen[DStream[A]]) : Gen[Seq[Batch[A]]] = gs.map(_.toSeq)
    implicit def batchSeqGen2dstreamGen[A](gs : Gen[Seq[Batch[A]]]) : Gen[DStream[A]] = gs.map(DStream(_))
  }
  case class DStream[A](batches : Seq[Batch[A]]) extends Seq[Batch[A]] {
    override def toSeq : Seq[Batch[A]] = batches
    
    override def apply(idx : Int) = batches.apply(idx)
    override def iterator = batches.iterator
    override def length = batches.length
  }
  
  object Buildables {
    /** Buildable for Seq, so we can use Gen.containerOf and Arbitrary.arbitrary
     *  using Seq as a container
     *  */
    implicit def buildableSeq[T] = new Buildable[T, Seq[T]] {
      def builder = new collection.mutable.Builder[T,Seq[T]] {
        var xs : List[T] = List() 
        def +=(x: T) = {
      	  xs = x :: xs
    	  this
        }
        def clear = xs = List()
        def result = xs reverse // note it is important to respect the generation order
      }
    }
    
    /** Builds a Buildable as a transformation of a given buildable, both element wise and
     *  on the container
     * */
    def mapBuildable[T1, C1[_], T2, C2[_]]
      (elemMapping : T2 => T1, resultMapping : C1[T1] => C2[T2])(buildable : Buildable[T1, C1[T1]])
      : Buildable[T2, C2[T2]] = new Buildable[T2, C2[T2]] { 
        def builder = new collection.mutable.Builder[T2,C2[T2]] {
          var nestedBuilder : collection.mutable.Builder[T1, C1[T1]] = buildable.builder
          def +=(x : T2) = {
            nestedBuilder += elemMapping(x)
            this
          }
          def clear = nestedBuilder.clear
          def result = nestedBuilder.mapResult(resultMapping).result
        }
    }

    /** Builds a Buildable as a transformation of a given buildable, by tranforming 
     *  the result with collection.mutable.Builder.mapResult
     * */
    def mapBuildable[T, R1, R2]
      (resultMapping : R1 => R2)(buildable : Buildable[T, R1])
      : Buildable[T, R2] = new Buildable[T, R2] { 
        def builder = new collection.mutable.Builder[T,R2] {
          var nestedBuilder : collection.mutable.Builder[T, R1] = buildable.builder
          def +=(x : T) = {
            nestedBuilder += x
            this
          }
          def clear = nestedBuilder.clear
          def result = nestedBuilder.mapResult(resultMapping).result
        }
    }
    
    /** A Buildable for building an object Batch[T] from its elements of type T
     * */
    implicit def buildableBatch[T] : Buildable[T, Batch[T]] =
        // alternative implementation based on the overload of mapBuildable, implies additional 
        // calls to indentity
      // mapBuildable(identity[T], (xs : List[T]) => Batch(xs))(implicitly[Buildable[T, List[T]]])
      mapBuildable((xs : List[T]) => Batch(xs))(implicitly[Buildable[T, List[T]]])
    
    /** A Buildable for building an object DStream[T] from its batches of type Batch[T]
     * */
    implicit def buildableDStreamFromBatch[T] : Buildable[Batch[T], DStream[T]] = 
      mapBuildable((batches : List[Batch[T]]) => DStream(batches))(implicitly[Buildable[Batch[T], List[Batch[T]]]])      
  }
 
  object BatchGen {
    /** @returns a generator of Batch that generates its elements from g
     * */
	def of[T](g : => Gen[T]) : Gen[Batch[T]] = {
	  import Buildables.buildableBatch
	  Gen.containerOf[Batch, T](g)  
	}
	
	/** @returns a generator of Batch that generates its elements from g
     * */
	def ofN[T](n : Int, g : Gen[T]) : Gen[Batch[T]] = {
	  import Buildables.buildableBatch
	  Gen.containerOfN[Batch, T](n, g)  
	}
	
	/** @returns a generator of Batch that generates its elements from g
     * */
	def ofNtoM[T](n : Int, m : Int, g : => Gen[T]) : Gen[Batch[T]] = {
	  import Buildables.buildableBatch
	  UtilsGen.containerOfNtoM[Batch, T](n, m, g)  
	}
  }
  
  object DStreamGen {
    def apply[A](dsg : Gen[DStream[A]]) : DStreamGen[A] = new DStreamGen(dsg)
    implicit def genDStream2DStreamGen[A](dsg : Gen[DStream[A]]) : DStreamGen[A] = DStreamGen(dsg)
    
    /** Arbitrary generator for DStream. Only works if an Arbitrary for Batch is also present. 
     *  Note this hasn't been automatically derived from Buildables.buildableDStreamFromBatch
     *  for the same reason Gen.buildableOf has to be used with DStream instead of Gen.containerOf
     * */
    implicit def arbDStream[A](implicit arbBatch : Arbitrary[Batch[A]]) : Arbitrary[DStream[A]] = 
      Arbitrary(DStreamGen.of(arbitrary[Batch[A]]))

    
    /** @returns a generator of DStream that generates its batches from bg
     * */
    def of[T](bg : => Gen[Batch[T]]) : Gen[DStream[T]] = {
      import Buildables.buildableDStreamFromBatch
      Gen.buildableOf[DStream[T], Batch[T]](bg)
    }
    /** @returns a generator of DStream that generates its batches from bg
     * */
    def ofN[T](n : Int, bg : Gen[Batch[T]]) : Gen[DStream[T]] = {
      import Buildables.buildableDStreamFromBatch
      Gen.buildableOfN[DStream[T], Batch[T]](n, bg)
    }
    /** @returns a generator of DStream that generates its batches from bg
     * */
    def ofNtoM[T](n : Int, m : Int, bg : Gen[Batch[T]]) : Gen[DStream[T]] = {
      import Buildables.buildableDStreamFromBatch
      UtilsGen.buildableOfNtoM[DStream[T], Batch[T]](n, m, bg)
    } 
  } 
  class DStreamGen[A](self : Gen[DStream[A]]) {
    /** Returns the generator that results from concatenating the sequences
     *  generated by the generator wrapped by this, and other 
     * */
    def ++(other : Gen[DStream[A]]) : Gen[DStream[A]] = UtilsGen.concSeq(self, other)
    
    /** @return a generator for the batch-by-batch concatenation of the dstreams 
    * generated by the generator wrapped by this, and other 
    * Note we fill empty batches of either dstreams. 
    * This implies dstreams are implicitly treated as they where infinitely extended 
    *  with empty batches  
    */
    def +(other : Gen[DStream[A]]) : Gen[DStream[A]] = dstreamUnion(self, other)
  }
}

package object sscheck  {
  // type Batch[A] = Seq[A] FIXME delete
  // type DStream[A] = Seq[Batch[A]] FIXME delete
  // def emptyBatch : Batch[Nothing] = new Batch(points = List()) // List() FIXME delete
 
  /** @return a generator for the batch-by-batch concatenation of the dstreams 
   *  generated by gs1 and gs2. Note we fill empty batches of either dstreams. 
   *  This implies dstreams are implicitly treated as they where infinitely extended 
   *  with empty batches  
   */
  def dstreamUnion[A](gs1 : Gen[DStream[A]], gs2 : Gen[DStream[A]]) : Gen[DStream[A]] = {
    for {
      xs1 <- gs1
      xs2 <- gs2
    } yield xs1. zipAll(xs2, Batch.empty, Batch.empty)
               . map(xs12 => xs12._1 ++ xs12._2) 
  }
}