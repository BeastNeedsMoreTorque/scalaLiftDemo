package code.model

import net.liftweb.common.Loggable

/**
  * Created by philippederome on 15-11-01.
  * This is captured from JSON parsing.
  */
case class Store(id: Int = 0,
                 is_dead: Boolean = true,
                 name: String = "",
                 distance_in_meters: Int = 0) extends Loggable {
  // intentional aliasing allowing more standard naming convention.
  val isDead = is_dead

  // intentional change of scale from metres to kilometres, using String representation instead of integer and keeping 3 decimals (0.335 ml for beer)
  def distanceInKMs: String = {
    val v = distance_in_meters.toInt / 1000.0
    f"$v%1.1f KM(s)"
  }

  override def toString = s"$id, $name; distance is:$distanceInKMs"
}
