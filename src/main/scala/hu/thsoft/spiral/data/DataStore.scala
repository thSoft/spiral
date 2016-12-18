package hu.thsoft.spiral.data

import hu.thsoft.spiral.data.Data.Stored
import monix.eval.Task
import monix.reactive.Observable
import upickle.Js

case class Invalid(dataStore: DataStore, json: Js.Value, expectedTypeName: String, error: Throwable)

/**
  * Represents a raw handle to an object stored in a storage.
  */
trait DataStore {

  def url: String

  def fromString(url: String): DataStore

  def child(name: String): DataStore

  type Serialized

  def observeAtomic[T](read: Serialized => T)(typeName: String): Observable[Stored[T]]

  def readDouble: Serialized => Double

  def observeNumber: Observable[Stored[Double]] = observeAtomic(readDouble)("number")

  def readString: Serialized => String

  def observeString: Observable[Stored[String]] = observeAtomic(readString)("string")

  def readBoolean: Serialized => Boolean

  def observeBoolean: Observable[Stored[Boolean]] = observeAtomic(readBoolean)("boolean")

  def observeChildren: Observable[List[DataStore]]

  def setAtomic[T](write: T => Serialized)(value: T): Task[Unit]

  def writeDouble: Double => Serialized

  def setNumber(value: Double): Task[Unit] = setAtomic(writeDouble)(value)

  def writeString: String => Serialized

  def setString(value: String): Task[Unit] = setAtomic(writeString)(value)

  def writeBoolean: Boolean => Serialized

  def setBoolean(value: Boolean): Task[Unit] = setAtomic(writeBoolean)(value)

  def delete: Task[Unit]

  def createChild: DataStore

  override def equals(obj: Any) = {
    obj match {
      case dataStore: DataStore => dataStore.url == url
      case _ => false
    }
  }

}
