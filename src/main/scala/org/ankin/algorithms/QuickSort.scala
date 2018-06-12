package org.ankin.algorithms

import scala.annotation.tailrec
import scala.io.Source

object QuickSort {

  def sort(array: Array[Int])(pivotFunc: (Array[Int], Int, Int) => (Int, Int)): Int = {
    sort(array, 0, array.length - 1)(pivotFunc)
  }


  def firstFunc(array: Array[Int], startIdx: Int, endIdx: Int): (Int, Int) = {
    (array(startIdx), startIdx)
  }

  def lastFunc(array: Array[Int], startIdx: Int, endIdx: Int): (Int, Int) = {
    val pivot = array(endIdx)
    array(endIdx) = array(startIdx)
    array(startIdx) = pivot
    (pivot, startIdx)
  }

  def medialFunc(array: Array[Int], startIdx: Int, endIdx: Int): (Int, Int) = {
    val length = endIdx - startIdx + 1
    val middleIdx = startIdx + (if (length % 2 == 0) {
      length / 2 - 1
    } else {
      length / 2
    })

    val list = List(array(startIdx), array(middleIdx), array(endIdx)).sorted
    val median = list(1)
    val medianIdx = (if (median == array(startIdx)) {
      startIdx
    } else if (median == array(endIdx)) {
      endIdx
    } else {
      middleIdx
    })


    val pivot = array(medianIdx)
    array(medianIdx) = array(startIdx)
    array(startIdx) = pivot
    (pivot, startIdx)

  }

  @tailrec
  private def sort(array: Array[Int], startIdx: Int, endIdx: Int)(implicit pivotFunc: (Array[Int], Int, Int) => (Int, Int)): Int = {
    if (endIdx - startIdx > 0) {
      val (pivot, initPivotIdx) = pivotFunc(array, startIdx, endIdx)
      var newPivotIdx = initPivotIdx
      for (i <- initPivotIdx + 1 to endIdx) {
        if (pivot > array(i)) {
          newPivotIdx = newPivotIdx + 1
          val tmpVal = array(newPivotIdx)
          array(newPivotIdx) = array(i)
          array(i) = tmpVal
        }
      }
      array(initPivotIdx) = array(newPivotIdx)
      array(newPivotIdx) = pivot

      sort(array, startIdx, newPivotIdx - 1) + sort(array, newPivotIdx + 1, endIdx) + endIdx - startIdx
    }
    else {
      0
    }

  }

  def main(args: Array[String]): Unit = {
    val arr = Source.fromFile("/Users/andrii/tmp/quicksort.txt").getLines().map(_.toInt).toArray
    println(s"Input: ${arr.mkString(", ")}")

    val startArr = arr.clone()
    println(s"Start # operations: ${sort(startArr)(firstFunc)}")
    println(startArr.mkString(", "))

    val endArr = arr.clone()
    println(s"End # operations: ${sort(endArr)(lastFunc)}")
    println(endArr.mkString(", "))

    val medianArr = arr.clone()
    println(s"Median # operations: ${sort(medianArr)(medialFunc)}")
    println(medianArr.mkString(", "))
  }
}
