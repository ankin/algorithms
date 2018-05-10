package org.ankin.algorithms

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable


object MergeSort {

  private val forkJoinPool = new ForkJoinPool
  private val scheduler =
    new DynamicVariable[DefaultTaskScheduler](new DefaultTaskScheduler)

  def sort(array: IndexedSeq[Int]): IndexedSeq[Int] = {
    mergeSort(array, 0, array.length)
  }

  private def mergeSort(list: IndexedSeq[Int], from: Int, to: Int): IndexedSeq[Int] = {
    if (to - from <= 1) {
      Vector(list(from))
    } else {
      val middle = (from + to) / 2
      val first = task {
        mergeSort(list, from, middle)
      }
      val second = task {
        mergeSort(list, middle, to)
      }
      merge(first.join(), second.join())

    }
  }

  private def merge(one: IndexedSeq[Int], two: IndexedSeq[Int]): IndexedSeq[Int] = {
    val iter = new Iterator[Int] {
      var oneIdx = 0
      var twoIdx = 0
      override def hasNext: Boolean = oneIdx != one.length || twoIdx != two.length
      override def next(): Int = if (oneIdx == one.length) {
        twoIdx = twoIdx + 1
        two(twoIdx - 1)
      } else if (twoIdx == two.length) {
        oneIdx = oneIdx + 1
        one(oneIdx - 1)
      } else if (one(oneIdx) <= two(twoIdx)) {
        oneIdx = oneIdx + 1
        one(oneIdx - 1)
      } else {
        twoIdx = twoIdx + 1
        two(twoIdx - 1 )
      }
    }
    iter.toIndexedSeq
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
    sort(Vector(2, 3, 2, 4, 1)).foreach(println(_))
  }

}

