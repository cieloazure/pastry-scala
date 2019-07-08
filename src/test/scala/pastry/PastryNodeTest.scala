package pastry

class PastryNodeTest extends UnitSpec {
  behavior of "Pastry Node"

  it should "when started without a seedEntry be the only node in the pastry network" in (pending)

  it should "when started with a seedEntry send a join request to the seed actor" in (pending)
  it should "when received a join request and all of its states are empty the routing logic will return" +
    "nothing and join response will be not okay" in (pending)
  it should "when join response is not okay request for all the states from the seed node and update its states " +
    "accordingly along with the entry for seed node" in (pending)
  it should "when received a join request and it's states have some nodes which the routing logic says is not null" +
    "then a route request will go to that node" in (pending)
  it should "when received a join request and it's states have some nodes but still the routing logic returns null" +
    "then a join response will be not okay" in (pending)
  it should "when received a join request and it's states have some nodes which the routing logic says is not null" +
    "but it is the same node which received the join request then a join request okay will be sent" in (pending)

  it should "when received a route request for a join and it's routing logic says there is a node then it forwards" +
    "to that node with new trace" in (pending)

  it should "when received a route request for a join and it's routing logic says there is a node but that node" +
    "the same node then routing join terminates and it sends back join response ok with trace " +
    "to new node" in (pending)

  it should "when received a route request for a join and it's routing logic says there is a node but that node" +
    "one in the trace then routing join terminates and it sends back the join response ok with trace to " +
    "new node" in (pending)

  it should "when join response not okay is received should request all three states of the node" in (pending)
  it should "when join response okay is received should request leaf set from last in the trace, nbhd set from first" +
    "in the trace and routing table rows from intermediate sets considering trace contains more than 2 " +
    "elements" in (pending)
  it should "when join response okay is received should request leaf set from last in the trace, nbhd set from first" +
    "in the trace and routing table rows from intermediate sets considering trace contains less than 2 " +
    "elements" in (pending)

  it should "on receiving leaf set request send it's leaf set" in (pending)
  it should "on receiving routing table request sent it's routing set" in (pending)
  it should "on receiving routing table request with idx send only that row of routing table" in (pending)
  it should "on receiving nbhd set request send it's nbhd" in (pending)

  it should "on receiving leaf set response update it's set"in (pending)
  it should "on receiving nbhd set response update it's set"in (pending)
  it should "on receiving routing table response update it's set"in (pending)

  it should "on receiving state responses with join purpose should send join complete message to every node " +
    "in the updated state"in (pending)

  it should "on receiving join complete update the new node in it's state if relevant" in (pending)
}
