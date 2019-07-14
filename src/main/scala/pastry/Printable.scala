package pastry

trait Printable {

  def bufferSpace: Unit = {
    for(_ <- 1 to 5) {
      println()
    }
  }
}
