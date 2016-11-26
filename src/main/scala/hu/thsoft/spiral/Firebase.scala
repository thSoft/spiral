package hu.thsoft.spiral

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import upickle.default.readJs
import upickle.default.writeJs

import hu.thsoft.firebase.Firebase
import hu.thsoft.firebase.FirebaseDataSnapshot
import hu.thsoft.spiral.Component.Action
import hu.thsoft.spiral.Data.Stored
import monix.execution.cancelables.BooleanCancelable
import monix.reactive.Observable
import monix.reactive.observables.ConnectableObservable
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.PublishSubject
import upickle.Js

case class Invalid(firebase: Firebase, json: Js.Value, expectedTypeName: String, error: Throwable)

case class Cancellation(cancellation: js.Any) extends Throwable

object Data {

  type Stored[T] = Either[Invalid, T]

  def observeRaw(firebase: Firebase, eventType: String = "value"): Observable[FirebaseDataSnapshot] =
    new ConnectableObservable[FirebaseDataSnapshot] {

      private val channel = PublishSubject[FirebaseDataSnapshot]

      private lazy val subscription = {
        val callback =
          (snapshot: FirebaseDataSnapshot, previousKey: js.UndefOr[String]) => {
            channel.onNext(snapshot)
            ()
          }
        val cancelCallback =
          (cancellation: js.Any) => {
            channel.onError(Cancellation(cancellation))
          }
        try {
          firebase.on(eventType, callback, cancelCallback)
        } catch {
          case e: Throwable => channel.onError(e)
        }
        BooleanCancelable(() => {
          channel.onComplete()
          firebase.off(eventType, callback)
        })
      }

      override def connect() = subscription

      override def unsafeSubscribeFn(subscriber: Subscriber[FirebaseDataSnapshot]) = {
        channel.unsafeSubscribeFn(subscriber)
      }

    }.refCount

  private def observeAtomic[T](readJson: Js.Value => T)(typeName: String)(firebase: Firebase): Observable[Stored[T]] =
    observeRaw(firebase).map(snapshot => {
      val snapshotValue = snapshot.`val`
      val json = upickle.json.readJs(snapshotValue)
      json match {
        case Js.Null => Left(Invalid(firebase, null, typeName, new NullPointerException))
        case _ =>
          try {
            Right(readJson(json))
          } catch {
            case e: Throwable => Left(Invalid(firebase, json, typeName, e))
          }
      }
    })

  def observeNumber(firebase: Firebase): Observable[Stored[Double]] =
    observeAtomic(readJs[Double])("number")(firebase)

  def observeString(firebase: Firebase): Observable[Stored[String]] =
    observeAtomic(readJs[String])("string")(firebase)

  def observeBoolean(firebase: Firebase): Observable[Stored[Boolean]] =
    observeAtomic(readJs[Boolean])("boolean")(firebase)

  private def setAtomic[T](writeJson: T => Js.Value)(firebase: Firebase, value: T): Action = () => {
    firebase.set(upickle.json.writeJs(writeJson(value)).asInstanceOf[js.Any])
  }

  def setNumber(firebase: Firebase, value: Double): Action = {
    setAtomic(writeJs[Double])(firebase, value)
  }

  def setString(firebase: Firebase, value: String): Action = {
    setAtomic(writeJs[String])(firebase, value)
  }

  def setBoolean(firebase: Firebase, value: Boolean): Action = {
    setAtomic(writeJs[Boolean])(firebase, value)
  }

}

abstract class Data(val firebase: Firebase) {

  def delete: Action = () => {
    firebase.remove()
  }

  override def equals(data: Any): Boolean = {
    data match {
      case data: Data => data.firebase.toString() == firebase.toString()
      case _ => false
    }
  }

}

abstract class AtomicData[Value](firebase: Firebase) extends Data(firebase) {

  def changed: Observable[Stored[Value]]

  def set(value: Value): Action

}

class NumberData(firebase: Firebase) extends AtomicData[Double](firebase) {

  def changed = Data.observeNumber(firebase)

  def set(value: Double) = Data.setNumber(firebase, value)

}

class StringData(firebase: Firebase) extends AtomicData[String](firebase) {

  def changed = Data.observeString(firebase)

  def set(value: String) = Data.setString(firebase, value)

}

class BooleanData(firebase: Firebase) extends AtomicData[Boolean](firebase) {

  def changed = Data.observeBoolean(firebase)

  def set(value: Boolean) = Data.setBoolean(firebase, value)

}

abstract class RecordData(firebase: Firebase) extends Data(firebase) {

  protected def newField[Field <: Data](name: String, makeData: Firebase => Field): Field = {
    makeData(firebase.child(name))
  }

}

abstract class ChoiceData[Choice](firebase: Firebase) extends Data(firebase) {

  protected def cases: Seq[Case[Choice]]

  def caseChanged: Observable[Stored[Choice]] = {
    val caseNameFirebase = caseNameChild(firebase)
    val caseNameObservable = Data.observeString(caseNameFirebase)
    caseNameObservable.map(storedCaseName => {
      storedCaseName.right.flatMap(
        typeName => {
          cases.find(_.name == typeName).map(foundCase => {
            val valueFirebase = valueChild(firebase)
            foundCase.makeChoice(valueFirebase)
          }).toRight(
            Invalid(caseNameFirebase, Js.Str(typeName), cases.map(_.name).mkString(" or "), new Exception(s"unknown $typeName"))
          )
        }
      )
    })
  }

  def setCase(caseName: String) = {
    new StringData(caseNameChild(firebase)).set(caseName)
  }

  private def caseNameChild(firebase: Firebase): Firebase = {
    firebase.child("case")
  }

  private def valueChild(firebase: Firebase): Firebase = {
    firebase.child("value")
  }

}

case class Case[Choice](name: String, makeChoice: Firebase => Choice)

class ReferenceData[Referred <: Data](firebase: Firebase)(makeData: Firebase => Referred) extends Data(firebase) {

  def referredChanged: Observable[Stored[Referred]] = {
    Data.observeString(firebase).map(storedUrl => {
      storedUrl.right.map(url => makeData(new Firebase(url)))
    })
  }

  def setReferred(referred: Referred): Action = {
    Data.setString(firebase, referred.firebase.toString())
  }

}

class ListData[Element <: Data](firebase: Firebase)(makeData: Firebase => Element) extends Data(firebase) {

  def changed: Observable[List[Element]] = {
    Data.observeRaw(firebase).map(snapshot => {
      getChildren(snapshot).map(makeData(_))
    })
  }

  private def getChildren(snapshot: FirebaseDataSnapshot): List[Firebase] = {
    val children = ListBuffer[Firebase]()
    snapshot.forEach((child: FirebaseDataSnapshot) => {
      children += child.ref()
      false
    })
    children.toList
  }

  def add(setNewElement: Element => Action): Action = () => {
    val child = firebase.push(null)
    setNewElement(makeData(child)).apply()
  }

}