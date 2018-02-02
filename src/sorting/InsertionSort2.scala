package sorting

/**
  * 関数型言語的な挿入ソート
  */
object InsertionSort2 extends Sorter {
  override def sort[A: Ordering](as: Seq[A]): Seq[A] = {
    val comparator = implicitly[Ordering[A]]
    def insert(value: A, lst: Seq[A]): Seq[A] = {
      lst match {
        // 挿入先が空 => 単一要素valueを持つq
        case Seq() => Seq(value)
        // 挿入先が空でない場合
        // valueが挿入先のheadより小さい or 等しい => valueの後に挿入先Seqを追加
        case Seq(h, _*) if comparator.lteq(value, h) => Seq(value) ++ lst
        // valueが挿入先のheadより大きい => headの後に、"valueをtailに挿入した結果"を追加
        case Seq(h, t @ _*) => Seq(h) ++ insert(value, t)
      }
    }

    // sort対象Seqの各要素を順に挿入していく
    as.foldRight(Seq.empty[A])(insert)
  }
}
