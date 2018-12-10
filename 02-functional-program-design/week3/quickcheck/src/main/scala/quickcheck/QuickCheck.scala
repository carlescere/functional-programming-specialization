package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("ins") = forAll { (h: H, a: Int, b: Int) =>
    val h1 = insert(a, insert(b, h))
    val h2 = insert(b, insert(a, h))

    findMin(h1) == findMin(h2)
    findMin(deleteMin(h1)) == findMin(deleteMin(h2))
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val expected =
      if (a < b) a
      else b
    findMin(h) == expected
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("del2") = forAll { h: H => {
    @tailrec
    def isMin(h: H, x: Int): Boolean =
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        if (x > min) false
        else isMin(deleteMin(h), min)
      }

    if (h == empty) true
    else isMin(deleteMin(h), findMin(h))
  }}

  property("meld1") = forAll { (h1: H, h2: H) => {
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) findMin(meld(h1,h2)) == findMin(h2)
    else if (isEmpty(h2)) findMin(meld(h1,h2)) == findMin(h1)
    else findMin(meld(h1,h2)) == findMin(h1) || findMin(meld(h1,h2)) == findMin(h2)
  }}

  property("meld2") = forAll { a: Int => {
    val h = meld(empty, insert(a, empty))
    findMin(h) == a
  }}

  property("empt") = forAll { a: Int => {
    isEmpty(empty) && !isEmpty(insert(a, empty))
  }}
}
