package hu.thsoft.spiral.data

import hu.thsoft.spiral.Component.Action
import hu.thsoft.spiral.data.Data.Stored
import monix.reactive.Observable
import upickle.Js

/**
  * Represents a type-safe handle to an object stored in a storage.
  */
abstract class Data(val dataStore: DataStore) {

  def delete: Action = dataStore.delete

  override def equals(obj: Any) = {
    obj match {
      case data: Data => (data.getClass == getClass) && (data.dataStore == dataStore)
      case _ => false
    }
  }

}

object Data {

  type Stored[T] = Either[Invalid, T]

}

abstract class AtomicData[Value](dataStore: DataStore) extends Data(dataStore) {

  def changed: Observable[Stored[Value]]

  def set(value: Value): Action

}

class NumberData(dataStore: DataStore) extends AtomicData[Double](dataStore) {

  def changed = dataStore.observeNumber

  def set(value: Double) = dataStore.setNumber(value)

}

class StringData(dataStore: DataStore) extends AtomicData[String](dataStore) {

  def changed = dataStore.observeString

  def set(value: String) = dataStore.setString(value)

}

class BooleanData(dataStore: DataStore) extends AtomicData[Boolean](dataStore) {

  def changed = dataStore.observeBoolean

  def set(value: Boolean) = dataStore.setBoolean(value)

}

abstract class RecordData(dataStore: DataStore) extends Data(dataStore) {

  protected def newField[Field <: Data](name: String, makeData: DataStore => Field): Field = {
    makeData(dataStore.child(name))
  }

}

abstract class ChoiceData[Choice](dataStore: DataStore) extends Data(dataStore) {

  protected def cases: Seq[Case[Choice]]

  def caseChanged: Observable[Stored[Choice]] = {
    val caseNameDataStore = caseNameChild(dataStore)
    val caseNameObservable = caseNameDataStore.observeString
    caseNameObservable.map(storedCaseName => {
      storedCaseName.right.flatMap(
        typeName => {
          cases.find(_.name == typeName).map(foundCase => {
            val valueDataStore = valueChild(dataStore)
            foundCase.makeChoice(valueDataStore)
          }).toRight(
            Invalid(caseNameDataStore, Js.Str(typeName), cases.map(_.name).mkString(" or "), new Exception(s"unknown $typeName"))
          )
        }
      )
    })
  }

  def setCase(caseName: String) = {
    new StringData(caseNameChild(dataStore)).set(caseName)
  }

  private def caseNameChild(dataStore: DataStore): DataStore = {
    dataStore.child("case")
  }

  private def valueChild(dataStore: DataStore): DataStore = {
    dataStore.child("value")
  }

}

case class Case[Choice](name: String, makeChoice: DataStore => Choice)

class ReferenceData[Referred <: Data](dataStore: DataStore)(makeData: DataStore => Referred) extends Data(dataStore) {

  def referredChanged: Observable[Stored[Referred]] = {
    dataStore.observeString.map(storedUrl => {
      storedUrl.right.map(url => makeData(dataStore.fromString(url)))
    })
  }

  def setReferred(referred: Referred): Action = {
    dataStore.setString(referred.dataStore.url)
  }

}

class ListData[Element <: Data](dataStore: DataStore)(makeData: DataStore => Element) extends Data(dataStore) {

  def changed: Observable[List[Element]] = dataStore.observeChildren.map(_.map(child => makeData(child)))

  def add(setNewElement: Element => Action): Action = () => {
    val child = dataStore.createChild
    setNewElement(makeData(child)).apply()
  }

}