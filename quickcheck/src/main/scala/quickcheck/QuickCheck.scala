package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.{forAll, _}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) Int.MinValue else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("insert delete") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("insert delete 2") = forAll { (a: Int, b: Int) =>
    !isEmpty(deleteMin(insert(b, insert(a, empty))))
  }

  property("insert max int") = forAll { h: H =>
    isEmpty(h) || findMin(insert(Int.MaxValue, h)) == findMin(h)
  }

  property("insert min int") = forAll { h: H =>
    isEmpty(h) || findMin(insert(Int.MinValue, h)) == Int.MinValue
  }

  def inOrder(h: H): Boolean = {
    def helper(h: H, prev_min: Int): Boolean = {
      isEmpty(h) ||
        (
          prev_min <= findMin(h) &&
            helper(deleteMin(h), findMin(h))
          )
    }
    helper(h, Int.MinValue)
  }

  property("minima") = forAll { h: H =>
    inOrder(h)
  }

  property("meld minimum") = forAll { (h1: H, h2: H) =>
    Set(findMin(h1), findMin(h2)).contains(findMin(meld(h1, h2)))
  }

  property("meld minima") = forAll { (h1: H, h2: H) =>
    inOrder(meld(h1, h2))
  }

  property("insert delete minima") = forAll { (h: H, a: Int, b: Int, c: Int) =>
    inOrder(deleteMin(insert(a, insert(b, insert(c, h)))))
  }

  property("meld insert delete minimum") = forAll { (h1: H, h2: H) =>
    findMin(meld(insert(findMin(h2), h1), deleteMin(h2))) == Math.min(findMin(h1), findMin(h2))
  }

  def toList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
  }

  def equals(h1: H, h2: H): Boolean = {
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (!isEmpty(h1) && !isEmpty(h2)) {
      findMin(h1) == findMin(h2) && equals(deleteMin(h1), deleteMin(h2))
    }
    else false
  }

  property("meld insert delete compare") = forAll { (h1: H, h2: H) =>
    equals(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}

