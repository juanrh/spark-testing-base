package es.ucm.fdi.sscheck.util

import org.scalacheck.util.Buildable

object Buildables {
  /** Buildable for Seq 
   */
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
}