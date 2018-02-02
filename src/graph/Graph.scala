package graph

import scala.collection.mutable
import scala.language.higherKinds

/**
  * A型の要素を頂点とするグラフ
  */
trait Graph[A] {
  type Edge[_]
  /**
    * グラフに辺 edge を追加する
    *
    * @return 辺が追加されたあとのグラフ
    */
  def add(edge: Edge[A]): this.type

  def +(edge: Edge[A]): this.type = add(edge)

  /**
    * グラフに複数の辺を追加する
    *
    * Usage:
    * val graph: Graph[Int] = new DirectedGraph[Int]
    * graph.addAll(1 -> 2, 1 -> 3, 2 -> 1)
    *
    * @return 辺が追加されたあとのグラフ
    */
  def addAll(edges: Edge[A]*): this.type = {
    for (e <- edges) add(e)
    this
  }

  def :++(edges: Edge[A]*): this.type = addAll(edges: _*)

  /**
    * グラフから辺 edge を削除する
    *
    * @return 辺が削除されたあとのグラフ
    */
  def remove(edge: Edge[A]): this.type

  def -(edge: Edge[A]): this.type = remove(edge)

  /**
    * グラフから複数の辺を削除する
    *
    * @return 辺が削除されたあとのグラフ
    */
  def removeAll(edges: Edge[A]*): this.type = {
    for (e <- edges) remove(e)
    this
  }

  def :--(edges: Edge[A]*): this.type = removeAll(edges: _*)

  /**
    * 辺の集合
    */
  def edges: mutable.Set[Edge[A]]

  /**
    * グラフが辺 edge を含むかどうかを判定する
    */
  def containsEdge(edge: Edge[A]): Boolean

  /**
    * 節点の集合
    */
  def vertices: Set[A]

  // このグラフの各節点について、V型の情報を記憶するMapを得る
  // 各エントリはinitValで初期化される
  private[graph] def initVerticesMap[V](initVal: V): mutable.Map[A, V] = {
    vertices.foldLeft(mutable.Map.empty[A, V])((acc, v) => acc + (v -> initVal))
  }

  /**
    * グラフが節点 v を含むかどうかを判定する
    */
  def contains(v: A): Boolean = vertices.contains(v)

  /**
    * 節点vに隣接する節点の集合
    */
  def neighborsOf(v: A): Set[A]

  /**
    * 節点vに接続する辺の集合
    */
  def incidentEdgesOf(v: A): Set[Edge[A]]


  /**
    * 節点fromから節点toまでの深さ優先探索による経路
    * @return 経路があれば、fromからtoまでの経路に含まれる節点からなるSeq。
    *         経路がなければ、空のSeq
    */
  def dfsPath(from: A, to: A): Seq[A] = BasicGraphSearch.dfs(this, from, to)

  /**
    * 節点fromからグラフの各節点までのBFS距離を計算する
    * @return
    */
  def bfsDistanceFrom(from: A): Map[A, Int] = BasicGraphSearch.calcBfsDistance(this, from)
}