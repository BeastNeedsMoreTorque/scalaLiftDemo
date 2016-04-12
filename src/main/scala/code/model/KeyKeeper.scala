package code.model

import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}

/**
  * Created by philippederome on 2016-04-12.
  */
trait KeyKeeper {
  def lcboId: LCBO_ID
  def pKey: P_KEY
}
