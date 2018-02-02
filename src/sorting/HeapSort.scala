package sorting

/**
  * ヒープソート
  */
object HeapSort extends Sorter {
  override def sort[A: Ordering](as: Seq[A]): Seq[A] = {
    val comp = Ordering[A]

    // as(i1) と as(i2) を入れ替えるヘルパーメソッド
    def swap(as: Seq[A], i1: Int, i2: Int): Seq[A] = {
      val a1 = as(i1)
      as.updated(i1, as(i2)).updated(i2, a1)
    }

    // i, lc, rcのうち一番大きい要素がある場所を求める
    // 結果は (子が親より大きいか?, 最大要素の場所)
    def checkMaxPos(as: Seq[A], i: Int, lc: Int, rc: Int, right: Int): (Boolean, Int) = {
      var maxP = i
      if (lc < right && comp.gt(as(lc), as(maxP))) maxP = lc
      if (rc < right && comp.gt(as(rc), as(maxP))) maxP = rc

      (maxP != i, maxP)
    }

    // as の [i, right) がヒープの条件を満たすように要素を入れ替える
    // ヒープの条件: すべての要素は、その子の要素よりも小さくない
    def heapify(as: Seq[A], i: Int, right: Int): Seq[A] = {
      // 左右の子の位置を求める
      val (lc, rc) = (i * 2 + 1, i * 2 + 2)

      // 子の要素 as(lc), as(rc) が as(i) よりも大きい場合
      // => 大きい方の子の要素と自分を入れ替えて、その子を根とする部分木を再帰的にheapify
      val (isChildGreat, maxPos) = checkMaxPos(as, i, lc, rc, right)
      if (isChildGreat) {
        heapify(swap(as, i, maxPos), maxPos, right)
      }
      // 自分が子要素より大きいなら入れ替えなし
      else as
    }

    // asの全体がヒープの条件を満たすようにする
    def buildHeap(as: Seq[A]): Seq[A] = {
      def loop(as: Seq[A], i: Int): Seq[A] = {
        if(i < 0) as
        else loop(heapify(as, i, as.length), i - 1)
      }

      loop(as, as.length/2 - 1)
    }

    // ソートのメインループ
    // 1. ヒープの根を未ソートの最後(=> 既ソートの最初)に持っていく
    // 2. 未ソート部を改めてヒープにする
    // 以上を繰り返す
    def mainLoop(as: Seq[A], i: Int): Seq[A] = {
      if(i < 1) as
      else {
        // 最初の要素(= ヒープの根 = ヒープ中の最大要素)を未ソート部の最後の要素と交換
        val swapped = swap(as, 0, i)
        // 交換後のasをheapifyしてから、未ソート部の右端を1つ左にずらして繰り返す
        mainLoop(heapify(swapped, 0, i), i - 1)
      }
    }

    val init = buildHeap(as)
    mainLoop(init, init.length - 1)
  }
}
