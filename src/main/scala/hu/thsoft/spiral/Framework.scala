package hu.thsoft.spiral

import org.scalajs.dom.raw.Node

import hu.thsoft.spiral.Component.Action
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.ReactElement
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

trait Component {

  type State

  def state: Observable[State]

  def output(state: State): Output

  def viewChanged: Observable[ReactElement] = state.switchMap(output(_).view)

  def reacted: Observable[Action] = state.switchMap(output(_).reaction)

}

object Component {

  type Action = Function0[Unit]

  def run(component: Component, container: Node) {
    component.viewChanged.foreach(element => ReactDOM.render(element, container))
    component.reacted.foreach(action => action.apply())
  }

}

case class Output(view: Observable[ReactElement], reaction: Observable[Action])

object Action {

  def nop = () => ()

  def sequential(actions: Seq[Action]): Action = () => {
    actions.foreach(_.apply)
  }

}

object ObservableUtils {

  /**
   * Similar to Observable.merge but accepts a sequence of Observables.
   */
  def merge[T](observables: Seq[Observable[T]]): Observable[T] = {
    Observable.merge(observables:_*)
  }

  /**
   * Similar to Observable.combineLatestList but accepts a sequence of Observables and if it is empty, emits an empty sequence.
   */
  def combineLatestList[T](observables: Seq[Observable[T]]): Observable[Seq[T]] = {
    if (observables.isEmpty) {
      Observable.pure(Seq())
    } else {
      Observable.combineLatestList(observables:_*)
    }
  }

  /**
   * Emits an item, then never completes.
   */
  def constant[T](item: T): Observable[T] = {
    Observable.never.startWith(Seq(item))
  }

  // TODO adapter for Observable so that flatMap has combineLatest semantics

}

case class Id(val segments: Seq[String]) {

  def child(segment: String): Id = {
    Id(segments :+ segment)
  }

  override def toString = segments.mkString("/")

}

object Id {

  def root: Id = Id(Seq())

}