package pastry

trait StateUpdatable {
  def updateState(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                  nodes: Array[Entry], idx: Option[Int] = None):
  (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]) = {

    val newLeafSet = state._1.update(nodes)
    val newRoutingTable = state._2.update(nodes)

    (newLeafSet, newRoutingTable, state._3)
  }

  def updateNeighbourhood(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                          nodes: Array[Entry]): (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]) = {
    val newNeighbourhoodSet = state._3.update(nodes)
    val newState = (state._1, state._2, newNeighbourhoodSet)
    newState
  }
}
