package pastry


class PastryRoutingTableTest extends UnitSpec {
  "Pastry Routing table" should "be initialized properly" in {
  }

  it should "on getTable return an array of any nodes specified in the table" in {
  }

  it should "on updateTable if the argument contains nodes which share prefix with the host update " +
    "appropriate entry in the table" in {
  }

  it should "on updateTable if the argument contains nodes which do not share prefix with the host not update " +
    "any rows in the table" in {
  }

  it should "on updateTable if the argument contains nodes which prefix with the host and that row already has max entries " +
    "then it should purge the entry with least priority" in {
  }

  it should "on getNode with a key and prefix function if that row is populated return a appropriate entry" in {
  }

  it should "on getNode with a key and prefix function if the key shares no prefix will return None" in {
  }

  it should "on getNode with a key and prefix function if the key shares prefix greater than maxRows will " in {
  }
}
