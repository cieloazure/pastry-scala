package pastry


class BoundedPriorityQueueTest extends UnitSpec {

  "BoundedPriorityQueue when created" should "when created be empty" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
  }

  "BoundedPriorityQueue when offered an element" should "while empty add an element to the list" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
    bpq.offer(1)
    assert(bpq.nonEmpty)
  }

  it should "while has more than one element add element in priority" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
    bpq.offer(1)
    bpq.offer(5)
    bpq.offer(2)
    bpq.offer(10)
    bpq.offer(0)
    assert(bpq.nonEmpty)
    val l = Array[Int](0,1,2,5,10)
    var m = Array[Int]()
    for(i <- 1 to 5){
      m = m ++ bpq.poll
    }
    assert(l sameElements m)
  }

  it should "while has more than one element add element in priority with a different comparator function" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b) * -1
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
    bpq.offer(1)
    bpq.offer(5)
    bpq.offer(2)
    bpq.offer(10)
    bpq.offer(0)
    assert(bpq.nonEmpty)
    val l = Array[Int](10, 5, 2, 1, 0)
    var m = Array[Int]()
    for(i <- 1 to 5){
      m = m ++ bpq.poll
    }
    assert(l sameElements m)
  }

  it should "while the queue is full and another element is added discard the element with lowest priority" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b) * -1
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
    bpq.offer(1)
    bpq.offer(5)
    bpq.offer(2)
    bpq.offer(10)
    bpq.offer(0)
    bpq.offer(3)
    assert(bpq.nonEmpty)
    val l = Array[Int]( 5, 3, 2, 1, 0)
    var m = Array[Int]()
    for(i <- 1 to 5){
      m = m ++ bpq.poll
    }
    assert(l sameElements m)
  }

  "BoundedPriorityQueue when polled for an element" should "while queue is empty, return None" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.poll.isEmpty)
  }

  it should "while has more than one element return that the least priority element" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    assert(bpq.isEmpty)
    bpq.offer(1)
    bpq.offer(5)
    bpq.offer(2)
    bpq.offer(10)
    bpq.offer(0)
    assert(bpq.nonEmpty)
    assert(bpq.poll.contains(0))
  }

  "BoundedPriorityQueue when offered multiple elements" should "take all those elements " +
    "and keep only elements with highest priority" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val bpq = new BoundedPriorityQueue[Int](5, comp)
    bpq.offer(1)
    bpq.offer(5)
    bpq.offer(2)
    bpq.offer(10)

    val newElems = Array[Int](-1, -2, -3)
    bpq.offerArray(newElems)
    val l = Array[Int](-1,1,2,5,10)
    var m = Array[Int]()
    for(i <- 1 to 5){
      m = m ++ bpq.poll
    }
    assert(l sameElements m)
  }
}
