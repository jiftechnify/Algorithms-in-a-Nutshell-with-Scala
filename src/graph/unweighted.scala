package graph

import scala.collection.mutable

object unweighted {
  type Edge[A] = (A, A)

  /**
    * 重みなしグラフ
    *
    * @tparam A 節点の型
    */
  trait UnweightedGraph[A] extends Graph[A] {
    type Edge[_] = (A, A)
    override val edges: mutable.Set[(A, A)] = mutable.Set.empty

    def edgesMap: mutable.MultiMap[A, A] = {
      val mm = new mutable.HashMap[A, mutable.Set[A]] with mutable.MultiMap[A, A]
      edges.groupBy(_._1).foldLeft(mm)((acc, e) => e match {
        case (from, edge) =>
          edge.foreach { case (_, to) => acc.addBinding(from, to) }
          acc
      })
    }

    def add(from: A, to: A): this.type = add(from -> to)

    def remove(from: A, to: A): this.type = remove(from -> to)

    def containsEdge(edge: Edge[A]): Boolean = edges.contains(edge)

    def neighborsOf(v: A): Set[A] = edgesMap.get(v).fold(Set.empty[A])(_.toSet)

    def incidentEdgesOf(v: A): Set[Edge[A]] = edges.filter { case (from, _) => v == from }.toSet

    override def toString: String =
      edgesMap.map { case (from, es) =>
        s"$from --> ${es.mkString("[", ", ", "]")}"
      }.mkString(System.lineSeparator())
  }

  /**
    * 有向(重みなし)グラフ
    */
  class DirectedGraph[A] extends UnweightedGraph[A] {
    override type Edge[_] = (A, A)
    def add(edge: Edge[A]): this.type = {
      edges += edge; this
    }

    def remove(edge: Edge[A]): this.type = {
      edges -= edge; this
    }

    def vertices: Set[A] = edgesMap.keySet.toSet ++ edgesMap.values.flatten
  }

  object DirectedGraph {
    def empty[A]: DirectedGraph[A] = new DirectedGraph[A]
    def apply[A](edges: Edge[A]*): DirectedGraph[A] = empty[A].addAll(edges: _*)
  }

  /**
    * 無向(重みなし)グラフ
    * 辺: from -> to が存在するならば、辺: to -> from も同時に存在する
    */
  class UndirectedGraph[A] extends UnweightedGraph[A] {
    private object EdgeUtil {
      def reverse(edge: Edge[A]): Edge[A] = edge match {
        case(f, t) => (t, f)
      }
    }

    def add(edge: Edge[A]): this.type = {
      edges += edge
      edges += EdgeUtil.reverse(edge)
      this
    }

    def remove(edge: Edge[A]): this.type = {
      edges -= edge
      edges -= EdgeUtil.reverse(edge)
      this
    }


    def vertices: Set[A] = edgesMap.keySet.toSet
  }

  object UndirectedGraph {
    def empty[A]: UndirectedGraph[A] = new UndirectedGraph[A]
    def apply[A](edges: Edge[A]*): UndirectedGraph[A] = UndirectedGraph.empty[A].addAll(edges: _*)
  }
}