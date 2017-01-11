package hu.thsoft.spiral.data

import firebase.database.{DataSnapshot, Database, Reference}
import monix.eval.Task
import monix.execution.cancelables.BooleanCancelable
import monix.reactive.Observable
import monix.reactive.observables.ConnectableObservable
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.PublishSubject
import upickle.Js
import upickle.Js.Value
import upickle.default.{readJs, writeJs}

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.|

case class Cancellation(cancellation: js.Any) extends Throwable

/**
  * Persistent and shared (Firebase-based) implementation of DataStore.
  */
class FirebaseDataStore(database: Database, firebase: Reference) extends DataStore {

  override type Serialized = Js.Value

  override def url = firebase.toString()

  private def observeRaw: Observable[DataSnapshot] =
    new ConnectableObservable[DataSnapshot] {

      private val channel = PublishSubject[DataSnapshot]

      private lazy val subscription = {
        val callback =
          (snapshot: DataSnapshot, previousKey: String | Null) => {
            channel.onNext(snapshot)
            ()
          }
        val cancelCallback =
          (cancellation: js.Any) => {
            channel.onError(Cancellation(cancellation))
          }
        val eventType = "value"
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

      override def unsafeSubscribeFn(subscriber: Subscriber[DataSnapshot]) = {
        channel.unsafeSubscribeFn(subscriber)
      }

    }.refCount

  override def observeAtomic[T](readJson: Js.Value => T)(typeName: String) =
    observeRaw.map(snapshot => {
      val snapshotValue = snapshot.`val`
      val json = upickle.json.readJs(snapshotValue)
      json match {
        case Js.Null => Left(Invalid(this, Js.Null, typeName, new NullPointerException))
        case _ =>
          try {
            Right(readJson(json))
          } catch {
            case e: Throwable => Left(Invalid(this, json, typeName, e))
          }
      }
    })

  override def fromString(url: String) = new FirebaseDataStore(database, database.refFromURL(url))

  override def child(name: String) = new FirebaseDataStore(database, firebase.child(name))

  override def observeChildren = {
    observeRaw.map(snapshot => {
      val children = ListBuffer[FirebaseDataStore]()
      snapshot.forEach((child: DataSnapshot) => {
        children += new FirebaseDataStore(database, child.ref)
        false
      })
      children.toList
    })
  }

  override def setAtomic[T](writeJson: (T) => Value)(value: T) = Task {
    firebase.set(upickle.json.writeJs(writeJson(value)).asInstanceOf[js.Any])
  }

  override def delete = Task {
    firebase.remove()
  }

  override def createChild = {
    new FirebaseDataStore(database, firebase.push(null))
  }

  override def readDouble = readJs[Double]

  override def readString = readJs[String]

  override def readBoolean = readJs[Boolean]

  override def writeDouble = writeJs[Double]

  override def writeString = writeJs[String]

  override def writeBoolean = writeJs[Boolean]
}