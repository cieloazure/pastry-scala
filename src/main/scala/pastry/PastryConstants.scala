package pastry

object PastryConstants {
  val B: Int = 3
  val BASE: Int = math.pow(2, B).toInt
  val NODES: Int = 64
  val LENGTH: Int = 4
  val DOUBLE_MODE: Boolean = false
  val DISTANCE_THRESHOLD: Int = 5
  object RoutingStatus {
    val FROM_LEAF_SET = 0
    val FROM_ROUTING_TABLE = 1
    val FROM_UNION = 2
  }
}
