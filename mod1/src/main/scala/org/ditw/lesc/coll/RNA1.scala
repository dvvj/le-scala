package org.ditw.lesc.coll

final class RNA1 private[coll] (
                           val groups:Array[Int],
                           val length:Int
                         ) extends IndexedSeq[Base] {
  import RNA1._

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
      case r:RNA1 => {
        r.groups.sameElements(groups) && r.length == length
      }
      case _ => false
    }
  }
}

object RNA1 {
  private val S = 2
  private val N = 32/S
  private val M = (1 << S)-1

  def fromSeq(buf:Seq[Base]):RNA1 = {
    val sz = (buf.length - 1) / N + 1
    val groups:Array[Int] = new Array[Int](sz)
    groups.indices.foreach(idx => groups(idx) = 0)
    buf.indices.foreach {i =>
      val idx = i/N
      groups(idx) |= Base.toInt(buf(i)) << (i % N * S)
    }
    new RNA1(groups, buf.length)
  }

  def apply(bases:Base*):RNA1 = fromSeq(bases)
}
