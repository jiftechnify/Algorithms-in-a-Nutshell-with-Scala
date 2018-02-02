package sorting

/**
  * クイックソート
  */
object QuickSort extends Sorter {
  override def sort[A: Ordering](as: Seq[A]): Seq[A] = {
    val comp = Ordering[A]

    // 空ならソート済みなのでそのまま返す
    if (as.isEmpty) as
    else {
      // 最初の要素をpivotとする
      val (pivot, rest) = (as.head, as.tail)
      // pivot以下の要素とpivotより大きい要素に分ける
      val (left, right) = rest.partition(comp.lteq(_, pivot))

      // pivot以下の要素をsortした結果 → pivot → pivotより大きい要素をsortした結果 の順に並べる
      sort(left) ++ Seq(pivot) ++ sort(right)
    }
  }
}
