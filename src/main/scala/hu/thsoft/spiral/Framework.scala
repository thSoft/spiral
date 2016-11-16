package hu.thsoft.spiral

import org.scalajs.dom.raw.Node

import hu.thsoft.spiral.Component.Action
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.ReactElement
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

case class Id(val segments: Seq[String]) {

  def child(segment: String): Id = {
    Id(segments :+ segment)
  }

  override def toString = segments.mkString("/")

}

object Id {

  def root: Id = Id(Seq())

}

object Action {

  def nop = () => ()

}

trait Component[S] {

  def state: Observable[S]

  def view(state: S): Observable[ReactElement]

  def viewChanged: Observable[ReactElement] = state.switchMap(view(_))

  def react(state: S): Observable[Action]

  def reacted: Observable[Action] = state.switchMap(react(_))

}

object Component {

  type Action = Function0[Unit]

  def run[S](component: Component[S], container: Node) {
    component.viewChanged.foreach(element => ReactDOM.render(element, container))
    component.reacted.foreach(action => action.apply())
  }

}