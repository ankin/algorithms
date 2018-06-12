package org.ankin.algorithms

import org.ankin.algorithms.QuickSort.{sort, firstFunc, lastFunc, medialFunc}
import org.scalatest.FlatSpec

import scala.io.Source

class QuickSortSpec extends FlatSpec {

  "A number of comparison with first pivot " should "be " in {
    val arr = Source.fromResource("quicksort.txt").getLines().map(_.toInt).take(100).toArray

    val comparisons = sort(arr)(firstFunc)
    assert(arr.deep == arr.sorted.deep)
    assert(comparisons == 620)
  }

  "A number of comparison with last pivot " should "be " in {
    val arr = Source.fromResource("quicksort.txt").getLines().map(_.toInt).take(100).toArray

    val comparisons = sort(arr)(lastFunc)
    assert(arr.deep == arr.sorted.deep)
    assert(comparisons == 573)
  }

  "A number of comparison with medial pivot " should "be " in {
    val arr = Source.fromResource("quicksort.txt").getLines().map(_.toInt).take(100).toArray

    val comparisons = sort(arr)(medialFunc)
    assert(arr.deep == arr.sorted.deep)
    assert(comparisons == 502)
  }


}
