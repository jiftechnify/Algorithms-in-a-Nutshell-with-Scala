package graph

import graph.unweighted.UnweightedGraph

import scala.collection.mutable

sealed trait SearchStatus
case object PreVisit extends SearchStatus
case object Visiting extends SearchStatus
case object Finished extends SearchStatus

object BasicGraphSearch {
//  def dfsAll[A](graph: Graph[A, _], start: A): mutable.Map[A, Option[A]] = {
//    val pred: mutable.Map[A, Option[A]] =
//      graph.vertices.foldLeft(mutable.Map.empty[A, Option[A]])((a, v) => a + (v -> None))
//    val status: mutable.Map[A, SearchStatus] =
//      graph.vertices.foldLeft(mutable.Map.empty[A, SearchStatus])((a, v) => a + (v -> PreVisit))
//
//    def dfsVisit(vertex: A): Unit = {
//      status(vertex) = Visiting
//
//      for(v <- graph.neighborsOf(vertex)) {
//        if (status(v) == PreVisit) {
//          pred(v) = Some(vertex)
//          dfsVisit(v)
//        }
//      }
//      status(vertex) = Finished
//    }
//
//    dfsVisit(start)
//    pred
//  }
//
//  /**
//    * 深さ優先探索を行い、2節点の間の経路を求める
//    * @param graph 探索するグラフ
//    * @param start 起点
//    * @param goal 終点
//    * @return 起点から終点までの経路を表すリスト。経路がなければ空リストを返す
//    */
//  def dfs[A](graph: Graph[A, _], start: A, goal: A): List[A] = {
//    val pred   = graph.vertices.foldLeft(mutable.Map.empty[A, Option[A]])((a, v) => a + (v -> None))
//    val status = graph.vertices.foldLeft(mutable.Map.empty[A, SearchStatus])((a, v) => a + (v -> PreVisit))
//
//    def dfsVisit(vertex: A): Unit = {
//      status(vertex) = Visiting
//
//      for(v <- graph.neighborsOf(vertex)) {
//        if (status(v) == PreVisit) {
//          pred(v) = Some(vertex)
//          if (v == goal) return else dfsVisit(v)
//        }
//      }
//
//      status(vertex) = Finished
//    }
//    dfsVisit(start)
//
//    def buildPath(vertex: A, path: List[A]): List[A] = {
//      if(vertex == start) path
//      else {
//        pred(vertex) match {
//          case None => List.empty
//          case Some(p) => buildPath(p, p :: path)
//        }
//      }
//    }
//    buildPath(goal, List(goal))
//  }
//
//  /**
//    * 関数型っぽい深さ優先探索
//    * @param graph 探索するグラフ
//    * @param start 起点
//    * @param goal 終点
//    * @return 起点から終点への経路
//    */
//  def dfs2[A](graph: Graph[A, _], start: A, goal: A): List[A] = {
//    def dfsVisit(curr: A, path: List[A]): List[A] = {
//      // 現在の節点の隣接点のうち、ここまでの経路に含まれない点
//      val nexts = graph.neighborsOf(curr) -- path
//      // 終点に到達したら見つかった経路を返す
//      if (curr == goal) goal :: path
//      // 行き止まりなら経路なしを表す空リスト
//      else if (nexts.isEmpty) Nil
//      // 未訪問の隣接点に進んでみる
//      // 空でないリストが得られれば探索成功 => 得られた経路を返す
//      // 全て空リストになったら探索失敗 => 空リストを返す
//      else
//        nexts.map(next => dfsVisit(next, curr :: path)).find(_.nonEmpty).getOrElse(Nil)
//    }
//    dfsVisit(start, List()).reverse
//  }

  /**
    * 幅優先探索によって、1つの節点から他の全節点への最短距離を求める
    * @param graph 探索するグラフ
    * @param from 起点となる節点
    * @return 各節点の、起点fromからの最短距離の表
    */
  def calcBfsDistance[A](graph: UnweightedGraph[A], from: A): Map[A, Int] = {
    val queue: mutable.Queue[A] = mutable.Queue(from)
    val status: mutable.Map[A, SearchStatus] = graph.initVerticesMap(PreVisit)
    val dist: mutable.Map[A, Int] = graph.initVerticesMap(-1)

    def bfsVisit(): Unit = {
      if(queue.isEmpty) return

      // キューの先頭にある節点を探索する
      val curr = queue.dequeue()
      if(status(curr) == PreVisit) {
        // 隣接する未訪問の節点をキューに追加
        val nexts = for (v <- graph.neighborsOf(curr) if status(v) == PreVisit) yield v
        queue.enqueue(nexts.toSeq: _*)
        // 最短距離を更新
        nexts.foreach(v => dist(v) = dist(curr) + 1)

        status(curr) = Finished
      }
      bfsVisit()
    }
    dist(from) = 0
    bfsVisit()

    // immutableなMapに変換
    dist.toMap
  }
}
