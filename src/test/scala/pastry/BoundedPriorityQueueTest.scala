package pastry

import scala.collection.mutable.ArrayBuffer

class BoundedPriorityQueueTest extends UnitSpec {
  "BoundedPriorityQueue when created" should "when created be empty" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b) < 0
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
  }

  "BoundedPriorityQueue when offered an element" should "while empty add an element to the list" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b) < 0
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
    val new_bpq = bpq.offer(1)
    assert(new_bpq.nonEmpty)
  }

  it should "while has more than one element add element in priority" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b) < 0
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
    val x = Array[Int](1,5,2,10,0)
    val new_bpq = x.foldLeft(new BoundedPriorityQueue[Int](5, comp))((acc, elem) => acc.offer(elem))
    assert(new_bpq.nonEmpty)
    val l = Array[Int](0,1,2,5,10)
    val m = Array[Int]()
    val m1 = removeAllElements(new_bpq, m)
    assert(l sameElements m1)
  }

  def removeAllElements(bpq:BoundedPriorityQueue[Int], acc: Array[Int]): Array[Int] = {
    bpq.poll match {
      case None => acc
      case Some((min, rest)) => removeAllElements(rest, acc :+ min)
    }
  }
}

