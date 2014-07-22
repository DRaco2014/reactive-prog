package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two el. heap") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (if (a <= b) a else b)
  }

  def emptyHeaps = value(empty)

  def nonEmptyHeaps = for {
    i <- arbitrary[Int]
    heaps <- genHeap
  } yield meld(insert(i, empty), heaps)

  lazy val genHeap: Gen[H] = for {
    isEmpty <- Arbitrary(oneOf(true, false)).arbitrary
    h <- (if (isEmpty) emptyHeaps else nonEmptyHeaps)
  } yield h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
