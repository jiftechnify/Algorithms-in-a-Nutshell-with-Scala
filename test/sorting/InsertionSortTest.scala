package sorting

import org.scalatest.FunSuite

class InsertionSortTest extends FunSuite {
  test("sort() sorts seq of int") {
    val seq = Seq(1, 9, 3, 7, 5, 4, 8, 2, 6)
    val sortedByInsertion = InsertionSort.sort(seq)
    assert(sortedByInsertion == seq.sorted)
  }

  test("sort() sorts seq of string") {
    val seq = Seq("banana", "orange", "apple", "grape")
    val sortedByInsertion = InsertionSort.sort(seq)
    assert(sortedByInsertion == seq.sorted)
  }
}
