package pastry


class PastryRoutingTableTest extends UnitSpec {
  "Pastry Routing Table" should "after creating a new instance be initialized to logN/log2^b" in {
    val routingTable: PastryRoutingTable = new PastryRoutingTable(PastryConstants.NODES, PastryConstants.TWO_RAISED_TO_BASE)
  }

//  it should "after calling add append the elements to the inner data structure" in {
//    import PastryRoutingTable.Entry
//    val routingTable: PastryRoutingTable = new PastryRoutingTable(PastryConstants.NODES, PastryConstants.TWO_RAISED_TO_BASE)
//    val newNodes: Array[Entry] = Array(Entry(1), Entry(2), Entry(3))
//
//    routingTable.add(newNodes, 0)
//
//    val newNodes2: Array[Entry] = Array(Entry(4), Entry(5), Entry(6))
//    routingTable.add(newNodes2, 1)
//  }
}
