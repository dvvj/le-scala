package org.ditw.lesc.coll

import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}
import scala.collection.mutable.ArrayBuffer

final class RNA3 private[coll] (
  val groups:Array[Int],
  val length:Int)
  extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA3]
{
  import RNA3._

  def apply(idx:Int):Base = {
    if (idx < 0 || length <= idx)
      throw new IndexOutOfBoundsException
    val grpIdx = idx / N
    val inGrpIdx = idx % N * S
    val baseVal = (groups(grpIdx) >> inGrpIdx) & M
    Base.fromInt(baseVal)
  }

  override def equals(obj:Any):Boolean = {
    obj match {
      case r:RNA3 => {
        r.groups.sameElements(groups) && r.length == length
      }
      case _ => false
    }
  }

  override protected[this] def newBuilder: mutable.Builder[Base, RNA3] =
    RNA3.newBuilder
}

object RNA3 {
  private val S = 2
  private val N = 32/S
  private val M = (1 << S)-1

  def fromSeq(buf:Seq[Base]):RNA3 = {
    val sz = (buf.length - 1) / N + 1
    val groups:Array[Int] = new Array[Int](sz)
    groups.indices.foreach(idx => groups(idx) = 0)
    buf.indices.foreach {i =>
      val idx = i/N
      groups(idx) |= Base.toInt(buf(i)) << (i % N * S)
    }
    new RNA3(groups, buf.length)
  }

  def newBuilder: mutable.Builder[Base, RNA3] =
    new ArrayBuffer[Base] mapResult fromSeq

  def apply(bases:Base*):RNA3 = fromSeq(bases)

  implicit def canBuildFrom:CanBuildFrom[RNA3, Base, RNA3] =
    new CanBuildFrom[RNA3, Base, RNA3] {
      def apply():mutable.Builder[Base, RNA3] = newBuilder
      def apply(from: RNA3):mutable.Builder[Base, RNA3] = newBuilder
    }
}
