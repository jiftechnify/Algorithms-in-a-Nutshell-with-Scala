package graph

import scala.collection.mutable

object weighted {
  /**
    * 重み付きグラフの辺
    */
  case class WeightedEdge[A](from: A, weight: Double, to: A) {
    override def toString: String = s"$from --|$weight|-> $to"

    def reversed = WeightedEdge(to, weight, from)

    // 等価性判定に重みは考慮しない!
    override def equals(that: scala.Any): Boolean = that match {
      case that: WeightedEdge[A] => this.from == that.from && this.to == that.to
      case _ => false
    }

    override def hashCode(): Int = {
      val p = 31
      var res = 1
      res = p * res + (if (from == null) 0 else from.hashCode())
      res = p * res + (if (to == null) 0 else to.hashCode())

      res
    }
  }

  /**
    * 節点 v1 から節点 v2 への重み w の辺を以下のように書けるようにする
    *
    * v1 --|w|-> v2
    */
  object WeightedEdgeSyntax {
    private[WeightedEdgeSyntax] case class PartialWEdge[A](from: A, weight: Double) {
      def |->(to: A): WeightedEdge[A] = WeightedEdge(from, weight, to)
    }

    implicit class WEdgeFrom[A](from: A) {
      def --|(weight: Double): PartialWEdge[A] = PartialWEdge(from, weight)
    }

    implicit class PairOps[A](pair: (A, A)) {
      def withWeight(weight: Double): WeightedEdge[A] = pair match {
        case (from, to) => WeightedEdge(from, weight, to)
      }
    }
  }

  trait WeightedGraph[A] extends Graph[A] {
    type Edge[_] = WeightedEdge[A]
    val edges: mutable.Set[WeightedEdge[A]] = mutable.Set.empty

    def edgesMap: mutable.MultiMap[A, WeightedEdge[A]] = {
      val mm = new mutable.HashMap[A, mutable.Set[WeightedEdge[A]]] with mutable.MultiMap[A, WeightedEdge[A]]
      edges.groupBy(_.from).foldLeft(mm)((acc, grp) => grp match {
        case (from, es) =>
          es.foreach(e => acc.addBinding(from, e))
          acc
      })
    }

    def add(from: A, to: A, weight: Double): this.type = add(WeightedEdge(from, weight, to))

    def remove(from: A, to: A, weight: Double): this.type = remove(WeightedEdge(from, weight, to))

    def containsEdge(edge: WeightedEdge[A]): Boolean = edges.contains(edge)

    def neighborsOf(vertex: A): Set[A] = edgesMap.get(vertex).fold(Set.empty[A])(_.map(_.to).toSet)

    def incidentEdgesOf(vertex: A): Set[WeightedEdge[A]] = edgesMap(vertex).toSet

    /**
      * このグラフから辺の重みだけを取り除き、節点の接続関係を保った重みなしグラフを返す
      */
    def unweighted: graph.unweighted.UnweightedGraph[A]

    override def toString: String = {
      edgesMap.map { case (from, es) =>
        s"$from --> ${es.map(e => s"${e.to}(${e.weight})").mkString("[", ", ", "]")}"
      }.mkString(System.lineSeparator())
    }
  }

  /**
    * 有向重み付きグラフ
    */
  class DirectedGraph[A] extends WeightedGraph[A] {
    def add(edge: WeightedEdge[A]): this.type = {
      edges += edge; this
    }

    def remove(edge: WeightedEdge[A]): this.type = {
      edges -= edge; this
    }

    def vertices: Set[A] = edges.flatMap(e => Set(e.from, e.to)).toSet

    def unweighted: graph.unweighted.DirectedGraph[A] = {
      val edges = this.edges.map(e => (e.from, e.to)).toSeq
      graph.unweighted.DirectedGraph(edges: _*)
    }
  }

  object DirectedGraph {
    def empty[A]: DirectedGraph[A] = new DirectedGraph[A]
    def apply[A](edges: WeightedEdge[A]*): DirectedGraph[A] = empty[A].addAll(edges: _*)
  }

  /**
    * 無向重み付きグラフ
    * 辺: from -> to が存在するならば、辺: to -> from も同時に存在する
    */
  class UndirectedGraph[A] extends WeightedGraph[A] {
    def add(edge: WeightedEdge[A]): this.type = {
      edges += edge
      edges += edge.reversed
      this
    }

    def remove(edge: WeightedEdge[A]): this.type = {
      edges -= edge
      edges -= edge.reversed
      this
    }

    val vertices: Set[A] = edgesMap.keySet.toSet

    def unweighted: graph.unweighted.UndirectedGraph[A] = {
      val edges = this.edges.map(e => (e.from, e.to)).toSeq
      graph.unweighted.UndirectedGraph(edges: _*)
    }
  }

  object UndirectedGraph {
    def empty[A]: UndirectedGraph[A] = new UndirectedGraph[A]
    def apply[A](edges: WeightedEdge[A]*): UndirectedGraph[A] = empty[A].addAll(edges: _*)
  }
}