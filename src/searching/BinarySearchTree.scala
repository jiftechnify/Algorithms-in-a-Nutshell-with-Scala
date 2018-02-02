package searching

trait BST[+A] {
  def isEmpty: Boolean

  def contains[B >: A : Ordering](target: B): Boolean
  def apply[B >: A : Ordering](target: B): Boolean = contains(target)

  def insert[B >: A : Ordering](e: B): BST[B]
  def +[B >: A : Ordering](e: B): BST[B] = insert(e)

  def remove[B >: A : Ordering](e: B): BST[B]
  def -[B >: A : Ordering](e: B): BST[B] = remove(e)

  def foreach(action: A => Unit): Unit

  def height: Int
  def heightDiff: Int
}
case object Nil extends BST[Nothing] {
  override def isEmpty: Boolean = true
  override def contains[B >: Nothing : Ordering](target: B): Boolean = false
  override def insert[B >: Nothing : Ordering](e: B): BST[B] = Node(e, Nil, Nil)
  override def remove[B >: Nothing : Ordering](e: B): BST[B] = Nil

  override def foreach(action: Nothing => Unit): Unit = () // Do nothing

  override def height: Int = 0
  override def heightDiff: Int = 0

  override def toString: String = "*"
}

case class Node[A: Ordering](value: A, left: BST[A], right: BST[A]) extends BST[A] {
  override def isEmpty: Boolean = false

  override def contains[B >: A : Ordering](target: B): Boolean = this match {
    case Node(v, l, r) =>
      val comp = Ordering[B]
      if (target == v)
        true
      else if (comp.lt(target, v))
        l.contains(target)
      else
        r.contains(target)
  }

  override def insert[B >: A : Ordering](e: B): BST[B] = this match {
    case Node(v, l, r) =>
      val comp = Ordering[B]
      if (e == v)
        this
      else if (comp.lt(e, v)) {
        val tmp = Node(v, l.insert(e), r)
        if(tmp.heightDiff == 2) {
          tmp match {
            case Node(_, Node(lv, _, _), _) if comp.lt(e, lv) => tmp.rotateR
            case _ => tmp.rotateLR
          }
        }
        else tmp
      }
      else {
        val tmp = Node(v, l, r.insert(e))
        if(tmp.heightDiff == -2) {
          tmp match {
            case Node(_, _, Node(rv, _, _)) if comp.gt(e, rv) => tmp.rotateL
            case _ => tmp.rotateRL
          }
        }
        else tmp
      }
  }

  override def remove[B >: A : Ordering](e: B): BST[B] = this match {
    case Node(v, l, r) =>
      val comp = Ordering[B]
      if (e == v) this.removeRoot()
      else if (comp.lt(e, v))
        Node(v, l.remove(e), r)
      else
        Node(v, l, r.remove(e))
  }

  private def removeRoot(): BST[A] = {
    def leftistElem(bst: BST[A]): A = bst match {
      case Node(v, Nil, _) => v
      case Node(_, l, _) => leftistElem(l)
    }

    this match {
      case Node(_, l, Nil) => l
      case Node(_, l, r) =>
        val leftist = leftistElem(r)
        Node(leftist, l, r - leftist)
    }
  }

  override def foreach(action: A => Unit): Unit = {
    left.foreach(action)
    action(this.value)
    right.foreach(action)
  }

  override def height: Int = math.max(left.height, right.height) + 1

  // 左右の部分木の高さの差
  override def heightDiff: Int = left.height - right.height

  // 右回転
  private def rotateR: BST[A] = this match {
    case Node(v, Node(v2, l2, r2), r) => Node(v2, l2, Node(v, r2, r))
  }

  // 右の子を軸に右回転してから左回転
  private def rotateRL: BST[A] = this match {
    case Node(v, l, Node(v2, Node(v3, l3, r3), r2)) =>
      Node(v3, Node(v, l, l3), Node(v2, r3, r2))
  }

  // 左回転
  private def rotateL: BST[A] = this match {
    case Node(v, l, Node(v2, l2, r2)) => Node(v2, Node(v, l2, l), r2)
  }

  // 左の子を軸に左回転してから右回転
  private def rotateLR: BST[A] = this match {
    case Node(v, Node(v2, l2, Node(v3, l3, r3)), r) =>
      Node(v3, Node(v2, l2, l3), Node(v, r3, r))
  }

  override def toString: String = s"[$left <$value> $right]"
}

object BST {
  def empty[A]: BST[A] = Nil
}