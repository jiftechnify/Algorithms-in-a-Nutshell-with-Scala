package searching

/**
  * ブルームフィルタに基づく探索が可能なSeq
  * @param hashFuncs
  * @param bitsSize
  * @tparam A
  */
case class BloomFilteredSeq[A](hashFuncs: Seq[A => Int], bitsSize: Int) {
  private val bits = Array.fill(bitsSize)(false)

  def search(target: A): Boolean = {
    for(hf <- hashFuncs) {
      val checkBit = hf(target)
      if(!bits(checkBit)) return false
    }
    true
  }

  def add(e: A): Unit = {
    for(hf <- hashFuncs) {
      val checkBit = hf(e)
      bits(checkBit) = true
    }
  }
}
