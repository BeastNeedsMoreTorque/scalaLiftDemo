package code.model


// to denote whether an Application Record requires to be inserted (New), updated (Dirty), or is good as is (Clean)
// We associate such state by comparing our entity with LCBO fresh data and place collections of entities
// in same bucket to do batch updates (insert on New or update on Dirty and no-op on Clean)
// to Squeryl. Undefined is when the data is unusable such as dependent on another entity for which we don't have access to data
// i.e. a foreign key that is missing (specifically an inventory received from LCBO about a product
// we don't have)
object EntityRecordState extends Enumeration {
  val New, Dirty, Clean, Undefined = Value
}
