package sorting

/**
  * マージソート
  */
object MergeSort extends Sorter {
  override def sort[A: Ordering](as: Seq[A]): Seq[A] = {
    val comp = Ordering[A]

    // 2つのSeqを要素が順に並ぶようにマージする
    def merge(as1: Seq[A], as2: Seq[A]): Seq[A] = {
      def mergeBody(as1: Seq[A], as2: Seq[A], acc: Seq[A]): Seq[A] = {
        (as1, as2) match {
          // どちらかが空 => 空でない方の残りの要素を全て追加
          case (Seq(), _) => acc ++ as2
          case (_, Seq()) => acc ++ as1
          // 両方に要素がある => 先頭の要素のうち小さい方を追加
          case (Seq(h1, t1 @ _*), Seq(h2, t2 @ _*)) =>
            if(comp.lteq(h1, h2))
              mergeBody(t1, as2, acc :+ h1)
            else
              mergeBody(as1, t2, acc :+ h2)
        }
      }
      mergeBody(as1, as2, Seq.empty)
    }

    // ソート処理本体
    def sortBody(as: Seq[A]): Seq[A] = {
      as match {
        // 要素が2個未満 => ソート済み
        case as if as.length < 2 => as
        // 要素が2個 => 順番が逆なら入れ替える
        case Seq(x, y) =>
          if(comp.lteq(x, y)) as
          else Seq(y, x)
        // 要素が3個以上 => 真ん中で分割してそれぞれをソートした後、マージする
        case _ =>
          val (l, r) = as.splitAt(as.length / 2)
          val (ls, rs) = (sortBody(l), sortBody(r))
          merge(ls, rs)
      }
    }
    sortBody(as)
  }
}
