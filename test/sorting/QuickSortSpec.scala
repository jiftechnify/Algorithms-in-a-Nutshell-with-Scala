package sorting

import org.scalatest.FlatSpec

class QuickSortSpec extends FlatSpec {
  "QuickSort" should "sort correctly a Seq of Int" in {
    val intSeq = Seq(1, 8, 3, 7, 2, 1, 8, 9, 3, 2)
    assert(QuickSort.sort(intSeq) == intSeq.sorted)
  }

  it should "sort correctly a Seq of String" in {
    val strSeq = Seq("banana", "orange", "grapefruit", "apple", "grape")
    assert(QuickSort.sort(strSeq) == strSeq.sorted)
  }
}
