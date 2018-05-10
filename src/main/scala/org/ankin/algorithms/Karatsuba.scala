package org.ankin.algorithms

object Karatsuba {

  implicit def toBigInt(v: IndexedSeq[Int]): BigInt = BigInt(v.mkString(""))
  implicit def toArray(v: BigInt): IndexedSeq[Int] = v.toString.map(_.asDigit)

  def multiply(value1: BigInt, value2: BigInt): BigInt = {
    if (value1 <= 10 && value2 <= 10) {
      // base case
      value1 * value2
    } else {
      val (array1, array2) = normalize(value1, value2)

      val (a, b) = array1.splitAt(array1.length / 2)
      val (c, d) = array2.splitAt(array2.length / 2)

      val ac = multiply(a, c)
      val bd = multiply(b, d)
      val sum = multiply(a + b, c + d)

      val power = Math.max(array1.length, array2.length)

      (ac * BigInt(10).pow(power)) + ((sum - ac - bd) * BigInt(10).pow(power / 2)) + bd
    }
  }

  def normalize(input1: IndexedSeq[Int], input2: IndexedSeq[Int]): (IndexedSeq[Int], IndexedSeq[Int]) = {
    val normalized1 = if (input1.length > 2 && input1.length % 2 != 0)
      0 +: input1
    else input1

    val normalized2 = if (input2.length > 2 && input2.length % 2 != 0)
      0 +: input2
    else input2

    makeSameLength(normalized1, normalized2)
  }

  def makeSameLength(input1: IndexedSeq[Int], input2: IndexedSeq[Int]): (IndexedSeq[Int], IndexedSeq[Int]) = {
    // align lengths
    if (input1.length < input2.length) {
      ((0 until input2.length - input1.length).map(_ => 0) ++ input1, input2)
    } else if (input1.length > input2.length) {
      (input1, (0 until input1.length - input2.length).map(_ => 0) ++ input2)
    } else (input1, input2)

  }

  def main(args: Array[String]): Unit = {
    val a = BigInt("1234")
    val b = BigInt("5678")
    println(s"$a*$b=${multiply(a, b)}")
  }
}
