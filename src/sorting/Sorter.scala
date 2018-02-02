package sorting

trait Sorter {
  def sort[A: Ordering](as: Seq[A]): Seq[A]
}
