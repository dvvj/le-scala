package org.ditw.lesc.coll

object SingletonTypeTest extends App {

  val str1 = "str1"
  val stStr1:str1.type = str1
  println(stStr1)

  // compilation error
//  val str2 = "str2"
//  val stStr2:str1.type = str2
//  println(stStr2)

//  var stStr1New = new str1.type()
//  println(stStr1New)

}
