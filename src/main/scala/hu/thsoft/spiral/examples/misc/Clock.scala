package hu.thsoft.spiral.examples.misc

import scala.scalajs.js.JSApp

import org.scalajs.dom._

import fr.hmil.roshttp.HttpRequest
import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Action
import hu.thsoft.spiral.Clickable
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.HTTP
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Output
import hu.thsoft.spiral.StringData
import hu.thsoft.spiral.examples.generic.ExampleUtils
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable
import upickle.Js
import upickle.json._
import scala.scalajs.js.Date
import hu.thsoft.spiral.Time
import org.scalajs.dom.ext.Color
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

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

object ClockApp extends JSApp {

  def main() {
    ExampleUtils.runComponent(new Clock)
  }

}