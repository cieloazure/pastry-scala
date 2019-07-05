package pastry


class RoutingTableTest extends UnitSpec {
  "Routing table" should "be initialized properly" in {
    val compFn = (a: String, b: String) => {
      a.toInt.compareTo(b.toInt)
    }

    val prefixFn = (a: String, b: String)  => {
      val c = a.stripPrefix(b)
      if(c == a){
        0
      }else{
        c.length
      }
    }

    val prt = new RoutingTable[String]("1", 4096, 16, compFn, prefixFn)
    assert(prt.rows == 3)
    assert(prt.entries == 16)
  }


  it should "on updateTable if the argument contains nodes which share prefix with the host update " +
    "appropriate entry in the table" in {
    val compFn = (a: String, b: String) => {
      a.toInt.compareTo(b.toInt)
    }

    val prefixFn = (a: String, b: String)  => {
      a.zip(b).takeWhile(_ match {case (x,y) => x == y}).length
    }

    val prt = new RoutingTable[String]("123456", 4096, 16, compFn, prefixFn)
    val nodes = Array[String]("100000", "120000", "123000")
    prt.updateTable(nodes)
    assert(prt.getNode("100001").contains("100000"))
    assert(prt.getNode("120001").contains("120000"))
    assert(prt.getNode("123001").contains("123000"))
  }

  it should "on updateTable if the argument contains nodes which do not share prefix with the host not update " +
    "any rows in the table" in {
    val compFn = (a: String, b: String) => {
      a.toInt.compareTo(b.toInt)
    }

    val prefixFn = (a: String, b: String)  => {
      a.zip(b).takeWhile(_ match {case (x,y) => x == y}).length
    }

    val prt = new RoutingTable[String]("123456", 4096, 16, compFn, prefixFn)
    val nodes = Array[String]("200000", "210000", "212000")
    prt.updateTable(nodes)
    assert(prt.getNode("200001").isEmpty)
    assert(prt.getNode("210001").isEmpty)
    assert(prt.getNode("212001").isEmpty)
  }

  it should "on updateTable if the argument contains nodes which prefix with the host and that row already has max entries " +
    "then it should purge the entry with least priority" is (pending)

  it should "on getNode with a key and prefix function if that row is populated return a appropriate entry" is (pending)

  it should "on getNode with a key and prefix function if the key shares no prefix will return None" is (pending)

  it should "on getNode with a key and prefix function if the key shares prefix greater than maxRows will " +
    "return an entry which shares maximum prefix if that row is not empty" is (pending)

  it should "on getTable return an array of any nodes specified in the table" in {
    val compFn = (a: String, b: String) => {
      a.toInt.compareTo(b.toInt)
    }

    val prefixFn = (a: String, b: String)  => {
      a.zip(b).takeWhile(_ match {case (x,y) => x == y}).length
    }

    val prt = new RoutingTable[String]("123456", 4096, 16, compFn, prefixFn)
    val nodes = Array[String]("100000", "120000", "123000")
    prt.updateTable(nodes)
    assert(prt.getTable sameElements nodes)
  }

  it should "on getTableRow get the specific row from the table" is (pending)
  it should "on updateTableRow set the particular row in the table" is (pending)
}
