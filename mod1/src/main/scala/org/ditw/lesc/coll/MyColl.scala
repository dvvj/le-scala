package org.ditw.lesc.coll

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{TraversableLike, mutable}


class MyColl[A](seq: Seq[A]) extends Traversable[A] with TraversableLike[A, MyColl[A]] {
  import MyColl._
  // only abstract method
  def foreach[U](f: A => U) =
    util.Random.shuffle(seq).foreach(f)

  override def newBuilder:mutable.Builder[A, MyColl[A]] = MyColl.newBuilder
}

object MyColl {
  def fromSeq[A](seq: Seq[A]):MyColl[A] = new MyColl(seq)
  def newBuilder[A]:mutable.Builder[A, MyColl[A]] =
    new ArrayBuffer[A] mapResult(fromSeq)

  implicit def canBuildFrom[A]:CanBuildFrom[MyColl[A], A, MyColl[A]] = new CanBuildFrom[MyColl[A], A, MyColl[A]] {
    override def apply(): mutable.Builder[A, MyColl[A]] = newBuilder[A]

    override def apply(from: MyColl[A]): mutable.Builder[A, MyColl[A]] = newBuilder[A]
  }
}

object MyCollTest extends App {
  val c = new MyColl(Seq(1, 2, 3))
  println(c mkString ",")
  println(c mkString ",")

  println(c drop 1 mkString ",")
  println(c drop 1 mkString ",")

  val c1 = c.drop(1)
  assert(c1.isInstanceOf[MyColl[_]])

  val c2 = c.map(_+1)
  assert(c2.isInstanceOf[MyColl[_]])
}