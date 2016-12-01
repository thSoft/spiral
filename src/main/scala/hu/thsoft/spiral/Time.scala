package hu.thsoft.spiral

import monix.reactive.Observable

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.Date

object Time {

  def current(period: FiniteDuration): Observable[Date] = {
    Observable.repeatEval(new Date).sample(period).startWith(Seq(new Date))
  }

}