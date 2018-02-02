package sorting

/**
  * 命令型言語的な挿入ソート
  */
object InsertionSort extends Sorter {
  override def sort[A: Ordering](as: Seq[A]): Seq[A] = {
    val comparator = Ordering[A]

    // valueを正しい位置に挿入する
    def insert(as: Seq[A], i: Int, value: A): Seq[A] = {
      if(comparator.lt(value, as(i))) {
        val swapped = as.updated(i + 1, as(i))
        if (i > 0) insert(swapped, i - 1, value) else swapped.updated(i, value)
      }
      else as.updated(i + 1, value)
    }

    // 「i番目の要素をソート済みの領域に挿入する」処理をi = 1 から as.length - 1 まで繰り返す
    def loop(as: Seq[A], i: Int): Seq[A] = {
      if(i >= as.length) as
      else {
        val inserted = insert(as, i - 1, as(i))
        loop(inserted, i + 1)
      }
    }

    loop(as, 1)
  }
}