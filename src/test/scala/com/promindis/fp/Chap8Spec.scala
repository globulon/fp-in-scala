package com.promindis.fp

import org.scalacheck.Gen._
import org.scalacheck.Properties
import org.scalacheck.Prop._

object Chap8Spec extends Properties("sum") {
  type sum =  List[Int] => Int
  type max = List[Int] => Int

   val aSum = (l: List[Int]) => l.sum
  val aMax = (l: List[Int]) => l.max

  //properties fpr sum
  property("ordereless") = forAll(listOf(choose(0,100))) { l => aSum(l) == aSum(l.reverse) }
  property("n repeat") =  forAll(listOfN(10, chooseNum(0,100))) { l => aSum(l) == 10 * l.head }
  //"empty lis sum is 0"

  property("max does not depemd on odrder") = forAll(listOf(choose(0,100))) { l => aMax(l) == aMax(l.reverse) }
  property("max is first when list repeat elements") = forAll(listOfN(10, chooseNum(0,100))) { l => aSum(l) == 10 * l.head }
  property("max is 0 when no element") = forAll() { l => aSum(l) == 10 * l.head }
}

