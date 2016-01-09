package code
package model

import net.liftweb.common._
import net.liftweb.http.SessionVar
import net.liftweb.mapper._
import net.liftweb.common.Box

// This is provided by Liftweb framework as a helper to get started or experiment.
/**
  * The singleton that has methods for accessing the database
  */
object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users"
  object storesCache extends SessionVar[Map[Int, Store]](Map.empty[Int, Store])

  // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
    <lift:bind/>
  </lift:surround>)


  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
    locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true

  def register(storeId: Int): Box[Store] = {
    if (storesCache.contains(storeId)) {
      Full(storesCache.get(storeId))
    } else {
      Store.find(storeId).map { s: Store => storesCache.set(storesCache.get + (storeId -> s)); s }
    }
  }
}

/**
  * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
  */
class User extends MegaProtoUser[User] {
  def getSingleton = User

  // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows = 10

    override def textareaCols = 50

    override def displayName = "Personal Essay"
  }

}

