package sorting

import org.scalatest.FlatSpec

class InsertionSort2Spec extends FlatSpec {
  "InsertionSort2" should "sort correctly a seq of int" in {
    val intSeq = Seq(1, 9, 4, 7, 3, 5, 2)
    assert(InsertionSort2.sort(intSeq) == intSeq.sorted)
  }

  it should "sort correctly a seq of String" in {
    val strSeq = Seq("apple", "orange", "grapefruit", "banana", "grape")
    assert(InsertionSort2.sort(strSeq) == strSeq.sorted)
  }
}
