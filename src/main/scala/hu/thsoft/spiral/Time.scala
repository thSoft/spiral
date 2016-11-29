package hu.thsoft.spiral

import scala.scalajs.js.Date

import monix.reactive.Observable
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

object Time {

  def current(period: FiniteDuration): Observable[Date] = {
    Observable.repeatEval(new Date).sample(period)
  }

}