package pastry

case class Location(val x: Float, val y: Float) {
  def distance(other: Location): Double = {
    math.sqrt(math.pow(this.x - other.x, 2) + math.pow(this.y - other.y, 2))
  }
}
