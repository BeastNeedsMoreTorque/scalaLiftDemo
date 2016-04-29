package code.model.utils

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import org.scalacheck.Gen
import org.scalacheck.Commands
import org.scalatest.prop.Checkers

/**
  * Created by philippederome on 2016-04-29.
  */
abstract class UnitTest extends FlatSpec with PropertyChecks with Matchers
