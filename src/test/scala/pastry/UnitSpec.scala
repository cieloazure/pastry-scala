package pastry

import org.scalatest._
import org.scalamock.scalatest.MockFactory

abstract class UnitSpec
  extends FlatSpec
  with Matchers
  with OptionValues
  with Inside
  with Inspectors
  with MockFactory
