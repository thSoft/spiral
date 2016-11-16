package hu.thsoft.spiral

import scala.scalajs._

import org.scalajs.dom._
import org.scalajs.dom.Event
import org.scalajs.dom.EventTarget
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.raw.HTMLSelectElement

import monix.execution.Ack
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.execution.cancelables.SingleAssignmentCancelable
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.Unbounded

object DOM {

  def eventListener(target: EventTarget, eventType: String): Observable[Event] =
    Observable.create(Unbounded) { subscriber =>
      val c = SingleAssignmentCancelable()
      // Forced conversion, otherwise canceling will not work!
      val f: js.Function1[Event,Ack] = (e: Event) =>
        subscriber.onNext(e).syncOnStopOrFailure(c.cancel())

      target.addEventListener(eventType, f)
      c := Cancelable(() => target.removeEventListener(eventType, f))
    }

  def on(id: Id, eventType: String): Observable[Event] = {
    eventListener(window, eventType).filter(event => {
      event.target match {
        case target: HTMLElement => target.id == id.toString
        case _ => false
      }
    })
  }

  def inputChanged(id: Id): Observable[String] = {
    on(id, "change").collect(event => {
      event.target match {
        case target: HTMLInputElement => target.value
      }
    })
  }

  def numericInputChanged(id: Id): Observable[Int] = {
    on(id, "change").collect(event => {
      event.target match {
        case target: HTMLInputElement =>
          target.valueAsNumber match {
            case number: Int => number
          }
      }
    })
  }

  def selectChanged(id: Id): Observable[String] = {
    on(id, "change").collect(event => {
      event.target match {
        case target: HTMLSelectElement => target.value
      }
    })
  }

}
