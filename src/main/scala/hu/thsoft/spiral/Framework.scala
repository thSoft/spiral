package hu.thsoft.spiral

import japgolly.scalajs.react.{ReactDOM, ReactElement}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalajs.dom.raw.Node

/**
  * The central concept of the framework.
  * A standalone component that can be displayed and reacts to user input.
  */
trait Component {

  type State

  /** The input of the component.
    * Warning: make sure that it never terminates,
    * so don't use Observable.empty, Observable.now or Observable.eval etc. to create it.
    */
  def state: Observable[State]

  /** The output of the component based on its state. */
  def output(state: State): Output

  /** Returns how the component looks like. */
  def viewChanged: Observable[ReactElement] = state.switchMap(output(_).view)

  /** Returns the reactions of the component. */
  def reacted: Observable[Task[_]] = state.switchMap(output(_).reaction)

}

object Component {

  /** Runs the given Component. It will be displayed in the given Node and its reactions will be performed.
    * Typically, you should call this only once in your JSApp's main method.
    */
  def run(component: Component, container: Node) {
    component.viewChanged.foreach(element => ReactDOM.render(element, container))
    component.reacted.foreach(action => action.runAsync)
  }

}

/**
  * Defines how a Component looks like and reacts.
  */
case class Output(view: Observable[ReactElement], reaction: Observable[Task[_]])

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

}

object TaskUtils {

  /** Returns a Task that executes the given Tasks unordered, discarding their results. */
  def gatherSideEffects[T](tasks: Seq[Task[T]]): Task[Unit] = {
    Task.gatherUnordered(tasks).map(_ => ())
  }

}

/**
  * A unique identifier composed of String segments to identify hierarchical entities.
  */
case class Id(val segments: Seq[String]) {

  /** Returns an Id with the given segment appended to this. */
  def child(segment: String): Id = {
    Id(segments :+ segment)
  }

  override def toString = {
    segments.mkString(Id.separator)
  }

}

object Id {

  val separator = "/"

  /** An empty Id. */
  def root: Id = Id(Seq())

  /** Parses an Id. You should use this very rarely. */
  def fromString(string: String): Id = Id(string.split(separator).toSeq)

}