package hu.thsoft.spiral

import monix.reactive.Observable

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.Date

object Time {

  /** Emits the current time on subscription, then periodically with the given period time. */
  def current(period: FiniteDuration): Observable[Date] = {
    Observable.repeatEval(new Date).sample(period).startWith(Seq(new Date))
  }

}