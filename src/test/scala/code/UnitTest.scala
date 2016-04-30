package code

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by philippederome on 2016-04-29.
  */
abstract class UnitTest extends FlatSpec with PropertyChecks with Matchers
