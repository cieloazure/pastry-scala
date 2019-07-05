package pastry

import scala.collection.mutable.ListBuffer

class PastryNodeIdTest extends UnitSpec {
  "PastryNodeId instance" should "after calling getHex with default parameter return correct hex string" in {

    val testCases:Map[String, String]
    =  Map(
      "The quick brown fox jumps over the lazy dog" -> "2fd4e1c67a2d28fced849ee1bb76e739",
      "The quick brown fox jumps over the lazy cog" -> "de9f2c7fd25e1b3afad3e85a0bd17d9b",
      "" -> "da39a3ee5e6b4b0d3255bfef95601890"
    )

    for((k,v) <- testCases) {
      val nodeId: PastryNodeId = new PastryNodeId(k)
      println(nodeId.getIntBase10)
      println(v.length)
      println(nodeId.getHex)
      assert(nodeId.getHex == v)
    }
  }

  it should "after calling getHex with given length return correct hex string with correct length" in {
    val testCases:Map[String, String]
    =  Map(
      "The quick brown fox jumps over the lazy dog" -> "2fd4",
      "The quick brown fox jumps over the lazy cog" -> "de9f",
      "" -> "da39"
    )

    for((k,v) <- testCases) {
      val nodeId: PastryNodeId = new PastryNodeId(k, 2)
      println(nodeId.getIntBase10)
      assert(nodeId.getHex == v)
    }
  }

  it should "after calling findCommonPrefix should return the bytes which are common" in {
    val nodeId1 = new PastryNodeId("The quick brown fox jumps over the lazy cog") //  -> "de9f",
    val nodeId2 = new PastryNodeId("") // -> "da39"
    val nodeId3 = new PastryNodeId("The quick brown fox jumps over the lazy dog") //  -> "de9f",

    assert(nodeId1.findCommonPrefix(nodeId2) == 1)
    assert(nodeId1.findCommonPrefix(nodeId3) == 0)
  }

}
