package pastry

class NeighbourhoodSetTest extends UnitSpec {
  "NeighbourhoodSet" should "on creating new instance get initialized" in {
    val ns = new NeighbourhoodSet[Int](5, 10, (a: Int, b: Int) => a.compareTo(b))
    assert(ns.isEmpty)
  }

  it should "update the set with elements provided" in {
    val ns = new NeighbourhoodSet[Int](5, 10, (a: Int, b: Int) => a.compareTo(b))
    val nodes = Array[Int](1,2,3,4)
    ns.updateSet(nodes)
    assert(ns.getSet sameElements nodes)
  }

  it should "update the set with elements provided and prevent overflow" in {
    val ns = new NeighbourhoodSet[Int](5, 3, (a: Int, b: Int) => a.compareTo(b))
    val nodes = Array[Int](1,2,3,4)
    ns.updateSet(nodes)
    assert(ns.getSet sameElements nodes.drop(1))
  }
}
