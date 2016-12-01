package hu.thsoft.spiral.localstorage

import java.util.UUID

import hu.thsoft.spiral.Component.Action
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.localstorage.Data.Stored
import monix.execution.cancelables.BooleanCancelable
import monix.reactive.Observable
import monix.reactive.observables.ConnectableObservable
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom
import org.scalajs.dom.StorageEvent

case class Invalid(id: Id, value: String, expectedTypeName: String, error: Throwable)

object Data {

  type Stored[T] = Either[Invalid, T]

  def observeRaw(id: Id): Observable[String] =
    new ConnectableObservable[String] {

      private val channel = PublishSubject[String]

      private lazy val subscription = {
        val callback =
          (event: StorageEvent) => {
            if (event.key == id.toString()) {
              channel.onNext(event.newValue.toString())
            }
            ()
          }
        val eventType = "storage"
        try {
          val currentValue = dom.window.localStorage.getItem(id.toString())
          channel.onNext(currentValue)
          dom.window.addEventListener(eventType, callback)
        } catch {
          case e: Throwable => channel.onError(e)
        }
        BooleanCancelable(() => {
          channel.onComplete()
          dom.window.removeEventListener(eventType, callback)
        })
      }

      override def connect() = subscription

      override def unsafeSubscribeFn(subscriber: Subscriber[String]) = {
        channel.unsafeSubscribeFn(subscriber)
      }

    }.refCount

  private def observeAtomic[T](readString: String => T)(typeName: String)(id: Id): Observable[Stored[T]] =
    observeRaw(id).map(value => {
      if (value == null) {
        Left(Invalid(id, null, typeName, new NullPointerException))
      } else {
        try {
          Right(readString(value))
        } catch {
          case e: Throwable => Left(Invalid(id, value, typeName, e))
        }
      }
    })

  def observeNumber(id: Id): Observable[Stored[Double]] =
    observeAtomic(_.toDouble)("number")(id)

  def observeString(id: Id): Observable[Stored[String]] =
    observeAtomic(identity)("string")(id)

  def observeBoolean(id: Id): Observable[Stored[Boolean]] =
    observeAtomic(_.toBoolean)("boolean")(id)

  def raiseStorageEvent(id: Id, newValue: String) {
    val oldValue = dom.window.localStorage.getItem(id.toString())
    val event = dom.document.createEvent("StorageEvent").asInstanceOf[StorageEvent]
    event.initStorageEvent("storage", true, true, id.toString(), oldValue, newValue, "", dom.window.localStorage)
    dom.window.dispatchEvent(event)
  }

  private def setAtomic[T](id: Id, value: T): Action = () => {
    val valueString = value.toString()
    dom.window.localStorage.setItem(id.toString(), valueString)
    raiseStorageEvent(id, valueString)
  }

  def setNumber(id: Id, value: Double): Action = {
    setAtomic(id, value)
  }

  def setString(id: Id, value: String): Action = {
    setAtomic(id, value)
  }

  def setBoolean(id: Id, value: Boolean): Action = {
    setAtomic(id, value)
  }

}

abstract class Data(val id: Id) {

  def delete: Action = () => {
    dom.window.localStorage.removeItem(id.toString())
    Data.raiseStorageEvent(id, "")
  }

  override def equals(data: Any): Boolean = {
    data match {
      case data: Data => data.id.toString() == id.toString()
      case _ => false
    }
  }

}

abstract class AtomicData[Value](id: Id) extends Data(id) {

  def changed: Observable[Stored[Value]]

  def set(value: Value): Action

}

class NumberData(id: Id) extends AtomicData[Double](id) {

  def changed = Data.observeNumber(id)

  def set(value: Double) = Data.setNumber(id, value)

}

class StringData(id: Id) extends AtomicData[String](id) {

  def changed = Data.observeString(id)

  def set(value: String) = Data.setString(id, value)

}

class BooleanData(id: Id) extends AtomicData[Boolean](id) {

  def changed = Data.observeBoolean(id)

  def set(value: Boolean) = Data.setBoolean(id, value)

}

abstract class RecordData(id: Id) extends Data(id) {

  protected def newField[Field <: Data](name: String, makeData: Id => Field): Field = {
    makeData(id.child(name))
  }

}

abstract class ChoiceData[Choice](id: Id) extends Data(id) {

  protected def cases: Seq[Case[Choice]]

  def caseChanged: Observable[Stored[Choice]] = {
    val caseNameKey = caseNameChild(id)
    val caseNameObservable = Data.observeString(caseNameKey)
    caseNameObservable.map(storedCaseName => {
      storedCaseName.right.flatMap(
        typeName => {
          cases.find(_.name == typeName).map(foundCase => {
            val valueId = valueChild(id)
            foundCase.makeChoice(valueId)
          }).toRight(
            Invalid(caseNameKey, typeName, cases.map(_.name).mkString(" or "), new Exception(s"unknown $typeName"))
          )
        }
      )
    })
  }

  def setCase(caseName: String) = {
    new StringData(caseNameChild(id)).set(caseName)
  }

  private def caseNameChild(id: Id): Id = {
    id.child("case")
  }

  private def valueChild(id: Id): Id = {
    id.child("value")
  }

}

case class Case[Choice](name: String, makeChoice: Id => Choice)

class ReferenceData[Referred <: Data](id: Id)(makeData: Id => Referred) extends Data(id) {

  def referredChanged: Observable[Stored[Referred]] = {
    Data.observeString(id).map(storedKey => {
      storedKey.right.map(key => makeData(Id.fromString(key)))
    })
  }

  def setReferred(referred: Referred): Action = {
    Data.setString(id, referred.id.toString())
  }

}

class ListData[Element <: Data](id: Id)(makeData: Id => Element) extends Data(id) {

  def changed: Observable[List[Element]] = {
    Data.observeRaw(id).map(value => {
      getChildren(value).map(key => makeData(Id.fromString(key)))
    })
  }

  private def getChildren(value: String): List[String] = {
    if (value == null) {
      List()
    } else {
      value.split(separator).toList
    }
  }

  val separator = "\n"

  def add(setNewElement: Element => Action): Action = () => {
    val childKey = id.child(UUID.randomUUID().toString())
    Data.setString(id, dom.window.localStorage.getItem(id.toString()) + separator + childKey.toString())
    setNewElement(makeData(childKey)).apply()
  }

}