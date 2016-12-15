package hu.thsoft.spiral.data

import hu.thsoft.spiral.Component.Action
import hu.thsoft.spiral.data.Data.Stored
import monix.reactive.Observable
import upickle.Js

/**
  * Represents a type-safe handle to an object stored in a storage.
  */
sealed abstract class Data(val dataStore: DataStore) {

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

class NumberData(dataStore: DataStore)(val min: Double, val max: Double) extends AtomicData[Double](dataStore) {

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

abstract class RecordData(dataStore: DataStore, val recordName: String) extends Data(dataStore) {

  protected def field[FieldData <: Data](name: String, makeData: DataStore => FieldData): Field[FieldData] = {
    Field(name, makeData(dataStore.child(name)))
  }

  def fields: Seq[Field[_ <: Data]]

}

case class Field[FieldData <: Data](name: String, data: FieldData)

case class CurrentCase[Choice <: Data](name: String, choice: Choice)

abstract class ChoiceData[Choice <: Data](dataStore: DataStore) extends Data(dataStore) {

  def cases: Seq[Case[Choice]]

  def caseChanged: Observable[Stored[CurrentCase[Choice]]] = {
    val caseNameDataStore = caseNameChild(dataStore)
    val caseNameObservable = caseNameDataStore.observeString
    caseNameObservable.map(storedCaseName => {
      storedCaseName.right.flatMap(
        caseName => {
          cases.find(_.name == caseName).map(foundCase => {
            CurrentCase(caseName, getValueData(foundCase))
          }).toRight(
            Invalid(caseNameDataStore, Js.Str(caseName), cases.map(_.name).mkString(" or "), new Exception(s"unknown $caseName"))
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

  def getValueData(selectedCase: Case[Choice]): Choice = {
    selectedCase.makeChoice(valueChild(dataStore))
  }
}

case class Case[Choice <: Data](name: String, makeChoice: DataStore => Choice)

abstract class ReferenceData[Referred <: Data](dataStore: DataStore)(makeData: DataStore => Referred) extends Data(dataStore) {

  def scope: Observable[List[Referred]]

  def getDisplayedName(referred: Referred): Observable[Stored[String]]

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