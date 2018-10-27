package org.ditw.lesc.coll

import scala.collection.mutable

object BuilderTests extends App {

  class ListBuilder[T] extends mutable.Builder[T, List[T]] {
    private var _storage = Vector.empty[T]

    // singleton type
    override def +=(elem: T): ListBuilder.this.type = {
      _storage = _storage :+ elem
      this
    }

    override def clear(): Unit = {
      _storage = Vector.empty[T]
    }

    override def result(): List[T] = _storage.toList
  }

  val lb = new ListBuilder[Int]
  (1 to 3).foreach (i => lb += i)
  val lr = lb.result()
  assert (lr == List(1, 2, 3))

  val m1 = Map("a" -> 1, "b" -> 2, "c" -> 2)
  val revM1 = m1.map {case (k, v) => v -> k}
  println(revM1)

}
