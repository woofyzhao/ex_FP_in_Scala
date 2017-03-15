package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
 * lazy val genMap: Gen[Map[Int,Int]] = oneOf(
 * const(Map.empty[Int,Int]),
 * for {
 * k <- arbitrary[Int]
 * v <- arbitrary[Int]
 * m <- oneOf(const(Map.empty[Int,Int]), genMap)
 * } yield m.updated(k, v)
 * )
 */

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      h <- genHeap
    } yield insert(k, h))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /*
   * If you insert any two elements into an empty heap, 
   * finding the minimum of the resulting heap should get the smallest of the two elements back.
   */
  property("2 elem") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a min b)
  }

  property("n elem1") = forAll { (xa: List[Int]) =>
    val h = xa.foldLeft(empty)((h: H, a: Int) => insert(a, h))
    if (!isEmpty(h)) findMin(h) == xa.min else xa.isEmpty
  }


  /*
   * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   +
   * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. 
   * (Hint: recursion and helper functions are your friends.
   */
  property("delete sole") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("delete all min") = forAll { (xa: List[Int]) =>
    val h = xa.foldLeft(empty)((h: H, a: Int) => insert(a, h))
    if (!isEmpty(h)) {

      def DelFromList(h: H, list: List[Int]): Boolean = {
        if (list.isEmpty) isEmpty(h) else {
          (findMin(h) == list.head) && DelFromList(deleteMin(h), list.tail)
        }
      }

      DelFromList(h, xa.sorted)
    } else xa.isEmpty
  }

  /*
   * Finding a minimum of the melding of any two heaps should return a minimum of one or the other
   */
  property("melding") = forAll { (ha: H, hb: H) =>
    val h = meld(ha, hb)
    val m = if (isEmpty(h)) 0 else findMin(h)
    if (isEmpty(ha) && isEmpty(hb)) m == 0
    else if (isEmpty(ha)) m == findMin(hb)
    else if (isEmpty(hb)) m == findMin(ha)
    else m == (findMin(ha) min findMin(hb))
  }
}
