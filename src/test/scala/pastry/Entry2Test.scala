package pastry

class Entry2Test extends UnitSpec {

  behavior of "Entry2Test"

  it should "toInt" in {
    val e1 = Entry2("73564275", null, null)
    assert(e1.toInt(8) == 15657149)
  }

  it should "toInt base 16" in {
    val e1 = Entry2("f7c096", null, null)
    assert(e1.toInt(16) == 17289366)
  }

  it should "toInt base 10" in {
    val e1 = Entry2("1234", null, null)
    assert(e1.toInt(10) == 1234)
  }
}
