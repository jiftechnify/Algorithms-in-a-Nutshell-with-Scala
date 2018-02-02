package searching

/**
  * ハッシュに基づく探索が可能なSeq
  * (ハッシュ値が被る場合はリストにする)
  *
  * ハッシュ値の被りが少なければ逐次探索のコストはO(1)と考えることができるので、平均計算量はO(1)
  * 最悪計算量はハッシュ値が全部被った場合のO(n)
  * => なるべくハッシュ値が被らないようなハッシュ関数を設計することが重要
  *
  * @param collection 初期コレクション
  * @param hashFunction ハッシュ表を作成するためのハッシュ関数
  */
case class HashedSeq[A](collection: Seq[A], hashFunction: A => Int) {
  // 初期コレクションからハッシュ表を作成
  // 計算量O(n)だが、1つのcollectionについて一度しか行われない
  private var hashTable: Map[Int, Seq[A]] = collection.groupBy(hashFunction)
  private var len = collection.length

  // 探索
  def search(target: A): Boolean = {
    // targetに対するハッシュ値を計算
    val hash = hashFunction(target)
    // ハッシュ表からtargetに対応するリストを取り出し、その中から逐次探索でtargetを探す
    hashTable.get(hash).fold(false)(SequentialSearch.search(_, target))
  }

  // 要素追加
  def add(e: A): Unit = {
    val hash = hashFunction(e)
    val a = hashTable.getOrElse(hash, Seq.empty[A]) :+ e
    hashTable += (hash -> a)
    len += 1
  }

  // 複数要素追加
  def addAll(es: Traversable[A]): Unit = {
    for(e <- es) add(e)
  }
  def addAll(es: A*): Unit = {
    addAll(es)
  }

  def length: Int = len
}

object HashedSeq {
  def empty[A](hashFunction: A => Int): HashedSeq[A] = HashedSeq(Seq.empty[A], hashFunction)
}

object HashBasedSearchApp extends App {
  val seq = Seq("hoge", "fuga", "poyo", "piyo", "hash", "based")

  val hashSeq = HashedSeq[String](seq, _.hashCode)
  Seq("hoge", "fuga", "baka").foreach(s => println(s"search $s: ${hashSeq.search(s)}"))

  hashSeq.add("baka")
  println(hashSeq.search("baka"))
}