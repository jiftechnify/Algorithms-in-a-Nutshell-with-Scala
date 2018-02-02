package sorting

import scala.collection.mutable.ListBuffer

/**
  * バケツソートで用いるバケツ
  */
class Bucket[A: Ordering] {
  private val elements: ListBuffer[A] = ListBuffer.empty
  def add(e: A): Unit = elements += e
  def sortedElements: Seq[A] = InsertionSort2.sort(elements)
}

/**
  * バケツソート(ハッシュソート)
  * この例では、英小文字からなる文字列の最初の3文字からハッシュを計算する
  */
object BucketSort {
  // ハッシュを計算する関数
  // 元の値の大小と、その値から計算されたハッシュの大小が一致するように定義する
  def hashString(s: String): Int =
    (s.charAt(0) - 'a') * 676 +
      (s.charAt(1) - 'a') * 26 +
      (s.charAt(2) - 'a')

  def bucketSort(ss: Seq[String]): Seq[String] = {
    // 必要な数のバケツを作成
    val buckets: Seq[Bucket[String]] =
      for(i <- 0 until 26 * 26 * 26) yield new Bucket[String]

    // ソート対象の各要素についてハッシュを計算し、対応するバケツに入れる
    for(s <- ss)
      buckets(hashString(s)).add(s)

    // 各バケツの内容をソートして、バケツの順序どおりにつなげる
    buckets.foldLeft(Seq.empty[String])((acc, b) => acc ++ b.sortedElements)
  }
}

object BucketSortApp extends App {
  val seq = Seq("foo", "bar", "baz", "hoge", "fuga", "afo", "baka", "orange", "banana", "bar", "apple", "pineapple")
  println(BucketSort.bucketSort(seq))
}
