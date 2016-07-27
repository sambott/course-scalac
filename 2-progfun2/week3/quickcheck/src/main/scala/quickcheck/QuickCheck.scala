package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = for {
      b <- arbitrary[Boolean]
      i <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield if (b) empty else insert(i: A, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: A) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == a.min(b)
  }

  property("remove min") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("sorted heap") = forAll { (h1: H, h2: H) =>
    @tailrec
    def inner(remaining: H, previous: Int): Boolean = {
      if (isEmpty(remaining)) true else {
        val next = findMin(remaining)
        if (next < previous) false else inner(deleteMin(remaining), next)
      }
    }
    inner(meld(h1, h2), Int.MinValue)
  }

  property("min of two") = forAll { (h1: H, h2: H, a1: Int, a2: Int) =>
    val h3 = insert(a1, h1)
    val h4 = insert(a2, h2)
    val h5 = meld(h3, h4)
    findMin(h5) == findMin(h4).min(findMin(h3))
  }

  def allElements(h: H): Set[A] = {
    @tailrec
    def inner(h1: H, acc: Set[A]): Set[A] = {
      if (isEmpty(h1)) acc else {
        val a = findMin(h1)
        val h2 = deleteMin(h1)
        inner(h2, acc + a)
      }
    }
    inner(h, Set.empty[A])
  }

  property("meld two has all correct elements") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    val allH1 = allElements(h1)
    val allH2 = allElements(h2)
    val allH3 = allElements(h3)
    allH3 == allH1 ++ allH2
  }


//  If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
//  If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
//  Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
//  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.


}
