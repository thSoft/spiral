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

  def sequential(actions: Seq[Action]): Action = () => {
    actions.foreach(_.apply)
  }

}

trait Component[State] {

  def state: Observable[State]

  def view(state: State): Observable[ReactElement]

  def viewChanged: Observable[ReactElement] = state.switchMap(view(_))

  def react(state: State): Observable[Action]

  def reacted: Observable[Action] = state.switchMap(react(_))

}

object Component {

  type Action = Function0[Unit]

  def run(component: Component[_], container: Node) {
    component.viewChanged.foreach(element => ReactDOM.render(element, container))
    component.reacted.foreach(action => action.apply())
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