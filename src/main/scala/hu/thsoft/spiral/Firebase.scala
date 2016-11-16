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

  type Stored[T]= Either[Invalid, T]

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

  def observeAtomic[T](readJson: Js.Value => T)(typeName: String)(firebase: Firebase): Observable[Stored[T]] =
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

  def observeString(firebase: Firebase): Observable[Stored[String]] =
    observeAtomic(readJs[String])("string")(firebase)

  def observeInt(firebase: Firebase): Observable[Stored[Int]] =
    observeAtomic(readJs[Int])("integer")(firebase)

  def observeDouble(firebase: Firebase): Observable[Stored[Double]] =
    observeAtomic(readJs[Double])("double")(firebase)

  def observeBoolean(firebase: Firebase): Observable[Stored[Boolean]] =
    observeAtomic(readJs[Boolean])("boolean")(firebase)

  def getChildren(snapshot: FirebaseDataSnapshot): List[Firebase] = {
    val children = ListBuffer[Firebase]()
    snapshot.forEach((child: FirebaseDataSnapshot) => {
      children += child.ref()
      false
    })
    children.toList
  }

  def caseNameChild(firebase: Firebase): Firebase = {
    firebase.child("case")
  }

  def valueChild(firebase: Firebase): Firebase = {
    firebase.child("value")
  }

}

abstract class Data(val firebase: Firebase)

abstract class AtomicData[Value](firebase: Firebase) extends Data(firebase) {

  def changed: Observable[Stored[Value]] = {
    Data.observeAtomic(readJson)(typeName)(firebase)
  }

  def readJson: Js.Value => Value

  def typeName: String

  def set(value: Value): Action = () => {
    firebase.set(upickle.json.writeJs(writeJson(value)).asInstanceOf[js.Any])
  }

  def writeJson: Value => Js.Value

}

class NumberData(firebase: Firebase) extends AtomicData[Double](firebase) {

  def readJson = readJs[Double]

  def typeName = "number"

  def writeJson = writeJs[Double]

}

class StringData(firebase: Firebase) extends AtomicData[String](firebase) {

  def readJson = readJs[String]

  def typeName = "string"

  def writeJson = writeJs[String]

}

class BooleanData(firebase: Firebase) extends AtomicData[Boolean](firebase) {

  def readJson = readJs[Boolean]

  def typeName = "boolean"

  def writeJson = writeJs[Boolean]

}

abstract class RecordData(firebase: Firebase) extends Data(firebase) {

  def newField[Field <: Data](name: String, makeData: Firebase => Field): Field = {
    makeData(firebase.child(name))
  }

}

abstract class ChoiceData[Choice](firebase: Firebase) extends Data(firebase) {

  def cases: Seq[Case[Choice]]

  def caseChanged: Observable[Stored[Choice]] = {
    val caseNameFirebase = Data.caseNameChild(firebase)
    val caseNameObservable = Data.observeString(caseNameFirebase)
    caseNameObservable.map(storedCaseName => {
      storedCaseName.right.flatMap(
        typeName => {
          cases.find(_.name == typeName).map(foundCase => {
            val valueFirebase = Data.valueChild(firebase)
            foundCase.makeChoice(valueFirebase)
          }).toRight(
            Invalid(caseNameFirebase, Js.Str(typeName), cases.map(_.name).mkString(" or "), new Exception(s"unknown $typeName"))
          )
        }
      )
    })
  }

  def setCase(caseName: String) = {
    new StringData(Data.caseNameChild(firebase)).set(caseName)
  }

}

case class Case[Choice](name: String, makeChoice: Firebase => Choice)
