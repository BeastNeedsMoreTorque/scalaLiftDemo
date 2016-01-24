package code.model

import scala.collection.Map

// to denote whether an Application Record requires to be inserted (New), updated (Dirty), or is good as is (Clean)
// We associate such state by comparing our entity with LCBO fresh data and place collections of entities
// in same bucket to do batch updates (insert on New or update on Dirty and no-op on Clean)
// to Squeryl.
trait EntityRecordState {}

object New extends EntityRecordState
object Dirty extends EntityRecordState
object Clean extends EntityRecordState
