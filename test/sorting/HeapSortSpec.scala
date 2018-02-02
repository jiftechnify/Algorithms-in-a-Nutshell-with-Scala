package sorting

import org.scalatest.FlatSpec

class HeapSortSpec extends FlatSpec {
  "Heapsort" should "sort correctly a seq of int" in {
    val intSeq = Seq(1, 9, 3, 5, 2, 7, 9, 6, 2, 1)
    assert(HeapSort.sort(intSeq) == intSeq.sorted)
  }

  it should "sort correctly a seq of String" in {
    val strSeq = Seq("banana", "grapefruit", "orange", "apple", "grape")
    assert(HeapSort.sort(strSeq) == strSeq.sorted)
  }
}
