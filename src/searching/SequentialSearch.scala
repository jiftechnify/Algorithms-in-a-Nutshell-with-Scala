package searching

/**
  * 逐次探索
  *
  * 計算量
  * 最良(最初の要素が所望の要素): O(1), 平均・最悪(見つからなかった場合): O(n)
  * 本来は探索対象の集合はTraversableであれば十分である。また、整列されている必要もない
  */
object SequentialSearch {
  def search[A](collection: Traversable[A], target: A): Boolean = {
    // 要素を順番に見ていき、targetに一致する要素があった時点でtrueを返す
    for(e <- collection) {
      if (e == target) return true
    }
    // 見つからなかった
    false
  }
}
