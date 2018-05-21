package org.ankin.algorithms

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable


object MergeSort {

  private val forkJoinPool = new ForkJoinPool
  private val scheduler =
    new DynamicVariable[DefaultTaskScheduler](new DefaultTaskScheduler)

  def sort(array: Array[Int]): (Long, Array[Int]) = {
    mergeSort(array, 0, array.length)
  }

  private def mergeSort(list: Array[Int], from: Int, to: Int): (Long, Array[Int]) = {
    if (to - from <= 1) {
      0L -> Array(list(from))
    } else {
      val middle = (from + to) / 2

      val firstTask = task {
        mergeSort(list, from, middle)
      }
      val (secondInvCount, secondArr) = task {
        mergeSort(list, middle, to)
      }.join()

      val (firstInvCount, firstArr) = firstTask.join()
      merge(firstArr, secondArr) match {
        case (splitInvCount, mergedArr) => (firstInvCount + secondInvCount + splitInvCount -> mergedArr)
      }

    }
  }

  private def merge(one: Array[Int], two: Array[Int]): (Long, Array[Int]) = {
    var inversionsCount = 0l
    val result = new Array[Int](one.length + two.length)

    var oneIdx = 0
    var twoIdx = 0
    var resIdx = 0

    while (oneIdx != one.length || twoIdx != two.length) {
      if (oneIdx == one.length) {
        result(resIdx) = two(twoIdx)
        twoIdx = twoIdx + 1
      } else if (twoIdx == two.length) {
        result(resIdx) = one(oneIdx)
        oneIdx = oneIdx + 1
      } else if (one(oneIdx) <= two(twoIdx)) {
        result(resIdx) = one(oneIdx)
        oneIdx = oneIdx + 1
      } else {
        inversionsCount = inversionsCount + one.length - oneIdx
        result(resIdx) = two(twoIdx)
        twoIdx = twoIdx + 1
      }
      resIdx = resIdx + 1
    }
    inversionsCount -> result
  }

  private class DefaultTaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  private def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def main(args: Array[String]): Unit = {
    sort(Array(2, 4, 1, 3, 5)) match {
      case (inversions, sorted) => println(s"Inversions: $inversions, sorted result: ${sorted.mkString(",")}")
    }
  }

}

