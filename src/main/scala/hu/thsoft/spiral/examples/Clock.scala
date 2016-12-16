package hu.thsoft.spiral.examples

import hu.thsoft.spiral.{Component, Output, Time}
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable
import org.scalajs.dom.ext.Color

import scala.concurrent.duration._
import scala.scalajs.js.Date

class Clock extends Component {

  type State = Date

  def state = Time.current(1 second)

  def output(state: State) = {
    val center = 50
    val radius = 40
    val clockFace = <.svg.circle(
      ^.svg.cx := center,
      ^.svg.cy := center,
      ^.svg.r := radius,
      ^.svg.fill := Color.White.toString(),
      ^.svg.stroke := Color.Black.toString()
    )
    def hand(clockAngle: Double, length: Double, color: Color) = {
      val angle = (clockAngle * 360 - 90).toRadians
      <.svg.line(
        ^.svg.x1 := center,
        ^.svg.y1 := center,
        ^.svg.x2 := center + length * Math.cos(angle),
        ^.svg.y2 := center + length * Math.sin(angle),
        ^.svg.stroke := color.toString()
      )
    }
    val hourHand = hand(state.getHours().toDouble / 12, radius * 0.8, Color.Black)
    val minuteHand = hand(state.getMinutes().toDouble / 60, radius * 0.95, Color.Black)
    val secondHand = hand(state.getSeconds().toDouble / 60, radius, Color.Red)
    val view: Observable[ReactElement] = Observable.pure(<.svg.svg(
      clockFace,
      hourHand,
      minuteHand,
      secondHand
    ))
    Output(view, Observable.empty)
  }

}