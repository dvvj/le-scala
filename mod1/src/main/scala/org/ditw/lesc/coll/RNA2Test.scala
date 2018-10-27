package org.ditw.lesc.coll

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RNA2Test extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  private val testData = Table(
    ("baseSeq", "expRNA"),
    (
      Seq(A, A, T, T, G, G, U, U),
      new RNA2(Array(0xFA50), 8)
    )
  )

  "RNA2 tests" should "pass" in {
    forAll(testData) { (baseSeq, expRNA) =>
      val rna2 = RNA2.fromSeq(baseSeq)

      rna2 shouldBe expRNA

      val t2 = rna2.take(2)
      t2.isInstanceOf[RNA2] shouldBe true

//      val t3 = rna2 ++ Seq(T, T)
//      t3.isInstanceOf[RNA2] shouldBe true

    }
  }

}
