package org.ankin.algorithms

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object MinCut {

  def main(args: Array[String]): Unit = {
    println(minCut(args(0), 50))
  }

  def minCut(graphFilePath: String, repeatTimes: Int): Int = {
    val mCut = (0 until repeatTimes)
      .map(_ => {
        val graphMap = parseFile(graphFilePath)
        contractGraph(graphMap).map {
          case (_, adjList) => adjList.size
        }.min
      })
      .min

    mCut
  }

  def parseFile(graphFilePath: String): Map[Long, List[Long]] = {
    Source.fromFile(graphFilePath).getLines()
      .filter(_.trim.nonEmpty)
      .map {
        case (line) => {
          val lineArr = line.split("\t").map(_.trim.toLong)
          lineArr(0) -> lineArr.drop(1).toList
        }
      } toMap
  }

  @tailrec
  def contractGraph(map: Map[Long, List[Long]]): Map[Long, List[Long]] = {
    map.size match {
      case 2 => map
      case _ => {
        val vertexDest = map.keys.toIndexedSeq(Random.nextInt(map.keys.size - 1))
        val destAdjacents = map(vertexDest)
        val vertexSource = destAdjacents.toIndexedSeq(Random.nextInt(destAdjacents.size - 1))
        contractGraph(contractVertices(vertexDest, vertexSource, map))
      }
    }
  }

  /**
    * @return new map representing a graph with merged vertices
    */
  def contractVertices(vertexDest: Long, vertexSource: Long, graphMap: Map[Long, List[Long]]): Map[Long, List[Long]] = {
    val destAdjacents = graphMap(vertexDest)
    val sourceAdjastens = graphMap(vertexSource)
    // merge two adjacency lists
    val mergedAdjacents = destAdjacents ++ sourceAdjastens
    // remove source vertex and replace destination vertex with merged adjacency list
    val updatedGraphMap = (graphMap - vertexSource) + (vertexDest -> mergedAdjacents)
    // replace all occurrences of source with destination vertex and remove self-loops
    updatedGraphMap.map {
      case (key, list) => {
        val updatedList = list.map(v => {
          if (v == vertexSource) vertexDest else v
        }).filter(_ != key)
        key -> updatedList
      }
    }
  }
}
