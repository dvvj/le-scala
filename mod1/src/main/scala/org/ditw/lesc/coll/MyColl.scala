package org.ditw.lesc.coll

class MyColl[A](seq: A*) extends Traversable[A] {
  // only abstract method
  def foreach[U](f: A => U) =
    util.Random.shuffle(seq.toSeq).foreach(f)
}

object MyCollTest extends App {
  val c = new MyColl(1, 2, 3)
  println(c mkString ",")
  println(c mkString ",")

  println(c drop 1 mkString ",")
  println(c drop 1 mkString ",")

  val c1 = c.drop(1)
  assert(c1.isInstanceOf[MyColl[_]])
}