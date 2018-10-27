package org.ditw.lesc.coll

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RNA1Test extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  private val testData = Table(
    ("baseSeq", "expRNA"),
    (
      Seq(A, A, T, T, G, G, U, U),
      new RNA1(Array(0xFA50), 8)
    )
  )

  "RNA1 tests" should "pass" in {
    forAll(testData) { (baseSeq, expRNA) =>
      val rna1 = RNA1.fromSeq(baseSeq)

      rna1 shouldBe expRNA

      // val t2 = rna1.take(2)
      // t2.isInstanceOf[RNA1] shouldBe true
    }
  }
}
