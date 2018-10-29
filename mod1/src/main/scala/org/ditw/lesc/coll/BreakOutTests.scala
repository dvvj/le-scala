package org.ditw.lesc.coll

object BreakOutTests extends App {

  import scala.collection.breakOut
  val map : Map[Int,String] =
    List("London", "France").map(x => (x.length, x))(breakOut)
  assert(map.isInstanceOf[Map[_, _]])

  println(map)
  val map1 =
    List("London", "France").map(x => (x.length, x))(breakOut)
  assert(map1.isInstanceOf[Iterable[_]])
  assert(!map1.isInstanceOf[Map[_, _]])
  val map2 =
    List("London", "France").map(x => (x.length, x))
  assert(map1 == map2)
  println(map1)


}
