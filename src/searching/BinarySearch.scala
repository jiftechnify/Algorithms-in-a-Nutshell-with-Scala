package searching

/**
  * 二分探索
  *
  * 計算量
  * 最良(真ん中が探す対象だった場合): O(1), 平均、最悪: O(log n)
  */
object BinarySearch {
  def search[A: Ordering](collection: Seq[A], target: A): Boolean = {
    val comp = Ordering[A]
    println(s"target: $target")

    // 探索処理本体
    def binSearch(coll: Seq[A], target: A, l: Int, h: Int): Boolean = {
      println("binSearch() called")
      if (l > h) false
      else {
        val mid = (l + h) / 2
        val cMid = coll(mid)
        if(cMid == target) true
        // 真ん中の要素がtargetより小さい => targetは右側にある
        else if(comp.lt(cMid, target))
          binSearch(coll, target, mid + 1, h)
        // 真ん中の要素がtargetより大きい => targetは左側にある
        else
          binSearch(coll, target, l, mid - 1)
      }
    }
    binSearch(collection, target, 0, collection.length - 1)
  }
}

object BinSearchApp extends App {
  val seq = Seq(1, 2, 3, 5, 8, 13, 21, 34, 55)
  Seq(1, 2, 3, 5, 8, 13, 21, 34, 55, 0).foreach(t => println(BinarySearch.search(seq, t)))
}