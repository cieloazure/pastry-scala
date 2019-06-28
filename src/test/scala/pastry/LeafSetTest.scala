package pastry

class LeafSetTest extends UnitSpec {
  "LeafSet" should "get initialized when a new instance is created" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val leafSet = new LeafSet[Int](5, 5, comp)
    assert(leafSet.isEmpty)
  }

  it should "update the lower and higher internal state when updateState is called" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val leafSet = new LeafSet[Int](5, 10, comp)
    val nodes = Array[Int](1,2,3,6,7,8)
    leafSet.updateSet(nodes)
    assert(leafSet.highest.contains(8))
    assert(leafSet.lowest.contains(1))
  }

  it should "return the entire set combined when getSet is called" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val leafSet = new LeafSet[Int](5, 10, comp)
    val nodes = Array[Int](1,2,3,6,7,8)
    leafSet.updateSet(nodes)
    assert(leafSet.getSet sameElements nodes)
  }

  it should "get the numerically closest node to a given key within range of leafSet when getNode is called" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val leafSet = new LeafSet[Int](5, 10, comp)
    val nodes = Array[Int](0,2,6,7,8)
    leafSet.updateSet(nodes)
    assert(leafSet.getNode(3,  (a: Int, b: Int) => a - b).contains(2))
  }

  it should "return None when given a key lesser than lower bound of leafSet when getNode is called" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val leafSet = new LeafSet[Int](5, 10, comp)
    val nodes = Array[Int](0,2,6,7,8)
    leafSet.updateSet(nodes)
    assert(leafSet.getNode(-1,  (a: Int, b: Int) => a - b).isEmpty)
  }

  it should "return None when given a key greater than upper bound of leafSet when getNode is called" in {
    val comp =  (a: Int, b: Int) => a.compareTo(b)
    val leafSet = new LeafSet[Int](5, 10, comp)
    val nodes = Array[Int](0,2,6,7,8)
    leafSet.updateSet(nodes)
    assert(leafSet.getNode(9,  (a: Int, b: Int) => a - b).isEmpty)
  }
}
