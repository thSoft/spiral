package hu.thsoft.spiral.data

import java.util.UUID

import hu.thsoft.spiral.Id
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.cancelables.BooleanCancelable
import monix.reactive.Observable
import monix.reactive.observables.ConnectableObservable
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom
import org.scalajs.dom.StorageEvent
import upickle.Js

/**
  * Persistent but not shared (LocalStorage-based) implementation of DataStore.
  */
class LocalDataStore(id: Id) extends DataStore {

  override type Serialized = String

  override def url = id.toString()

  private def observeRaw: Observable[String] =
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

  override def observeAtomic[T](readString: String => T)(typeName: String) =
    observeRaw.map(value => {
      if (value == null) {
        Left(Invalid(this, Js.Null, typeName, new NullPointerException))
      } else {
        try {
          Right(readString(value))
        } catch {
          case e: Throwable => Left(Invalid(this, Js.Str(value), typeName, e))
        }
      }
    })

  private def raiseStorageEvent(newValue: String) {
    val oldValue = dom.window.localStorage.getItem(id.toString())
    val event = dom.document.createEvent("StorageEvent").asInstanceOf[StorageEvent]
    event.initStorageEvent("storage", true, true, id.toString(), oldValue, newValue, "", dom.window.localStorage)
    dom.window.dispatchEvent(event)
  }

  override def setAtomic[T](writeString: T => String)(value: T) = Task {
    val valueString = value.toString()
    dom.window.localStorage.setItem(id.toString(), valueString)
    raiseStorageEvent(valueString)
  }

  override def fromString(url: String) = new LocalDataStore(Id.fromString(url))

  override def child(name: String) = new LocalDataStore(id.child(name))

  override def observeChildren = {
    observeRaw.map(value => {
      if (value == null) {
        List()
      } else {
        value.split(separator).map(fromString(_)).toList
      }
    })
  }

  val separator = "\n"

  override def delete = Task {
    dom.window.localStorage.removeItem(id.toString())
    raiseStorageEvent("")
  }

  override def createChild = {
    val childKey = id.child(UUID.randomUUID().toString())
    setString(dom.window.localStorage.getItem(id.toString()) + separator + childKey.toString()).runAsync
    new LocalDataStore(childKey)
  }

  override def readDouble = _.toDouble

  override def readString = _.toString

  override def readBoolean = _.toBoolean

  override def writeDouble = _.toString()

  override def writeString = _.toString()

  override def writeBoolean = _.toString()

}