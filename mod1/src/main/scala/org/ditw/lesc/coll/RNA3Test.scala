package org.ditw.lesc.coll

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RNA3Test extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  private val testData = Table(
    ("baseSeq", "expRNA", "take4RNA", "++TT"),
    (
      Seq(A, A, T, T, G, G, U, U),
      new RNA3(Array(0xFA50), 8),
      new RNA3(Array(0x50), 4),
      new RNA3(Array(0x5FA50), 10)
    )
  )

  "RNA3 tests" should "pass" in {
    forAll(testData) { (baseSeq, expRNA, take4RNA, plusplusTT) =>
      val rna3 = RNA3.fromSeq(baseSeq)

      rna3 shouldBe expRNA

      val t2 = rna3.take(4)
      t2 shouldBe take4RNA

      val t3 = rna3 ++ Seq(T, T)
      t3 shouldBe plusplusTT

    }
  }


}
