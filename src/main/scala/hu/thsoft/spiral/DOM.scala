package hu.thsoft.spiral

import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{CompState, ReactElement, ReactEventAliases}
import monix.execution.Scheduler.Implicits.global
import monix.execution.cancelables.SingleAssignmentCancelable
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.Unbounded
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement, HTMLSelectElement}
import org.scalajs.dom.{Event, EventTarget, _}

import scala.scalajs._

abstract class Interactive(protected val id: Id)(protected val tagMod: TagMod*) {

  def view: ReactElement = {
    makeElement((^.id := id.toString()) + tagMod + extraTagMod)
  }

  def makeElement(tagMod: TagMod): ReactElement

  def extraTagMod: TagMod = EmptyTag

}

class Checkbox(id: Id, state: Boolean)(tagMod: TagMod*) extends Interactive(id)(tagMod) {

  def makeElement(tagMod: TagMod) = <.input.checkbox(tagMod)

  override def extraTagMod = DOM.checkedAttribute(state)

  def changed: Observable[Boolean] = {
    DOM.on(id, "click").collect(event => {
      event.target match {
        case target: HTMLInputElement => target.checked
      }
    })
  }

}

abstract class TextualInput(id: Id, state: String)(tagMod: TagMod*) extends Interactive(id)(tagMod) {

  override def extraTagMod = DOM.valueAttribute(state)

  def changed: Observable[String] = {
    DOM.on(id, "change").collect(event => {
      event.target match {
        case target: HTMLInputElement => target.value
      }
    })
  }

}

class TextInput(id: Id, state: String)(tagMod: TagMod*) extends TextualInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.text(tagMod)
}

class Textarea(id: Id, state: String)(tagMod: TagMod*) extends TextualInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.textarea(tagMod)
}

class SearchInput(id: Id, state: String)(tagMod: TagMod*) extends TextualInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.search(tagMod)
}

class PasswordInput(id: Id, state: String)(tagMod: TagMod*) extends TextualInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.password(tagMod)
}

class EmailInput(id: Id, state: String)(tagMod: TagMod*) extends TextualInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.email(tagMod)
}

class TelephoneInput(id: Id, state: String)(tagMod: TagMod*) extends TextualInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.tel(tagMod)
}

class UrlInput(id: Id, state: String)(tagMod: TagMod*) extends TextualInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.url(tagMod)
}

abstract class NumericInput(id: Id, state: Double)(tagMod: TagMod*) extends Interactive(id)(tagMod) {

  override def extraTagMod = DOM.valueAttribute(state)

  def changed: Observable[Double] = {
    DOM.on(id, "change").collect(event => {
      event.target match {
        case target: HTMLInputElement => target.valueAsNumber
      }
    })
  }

}

class NumberInput(id: Id, state: Double)(tagMod: TagMod*) extends NumericInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.number(tagMod)
}

class RangeInput(id: Id, state: Double)(tagMod: TagMod*) extends NumericInput(id, state)(tagMod) {
  def makeElement(tagMod: TagMod) = <.input.range(tagMod)
}

class ChoiceList[Item](id: Id, choices: Seq[Choice[Item]], selectedItem: Item)(tagMod: TagMod*) extends Interactive(id)(tagMod) {

  def makeElement(tagMod: TagMod) = <.select(tagMod)

  override def extraTagMod = {
    val value = DOM.valueAttribute(choices.map(_.item).indexOf(selectedItem))
    val choiceViews = choices.zipWithIndex.map { case (choice, index) =>
      <.option((^.value := index) + choice.label)
    }
    value + choiceViews
  }

  def changed: Observable[Item] = {
    DOM.on(id, "change").collect(event => {
      event.target match {
        case target: HTMLSelectElement => choices(target.selectedIndex).item
      }
    })
  }

}

case class Choice[Item](item: Item, label: String)

class Clickable(doMakeElement: TagMod => ReactElement)(id: Id)(tagMod: TagMod*) extends Interactive(id)(tagMod) {

  def makeElement(tagMod: TagMod) = doMakeElement(tagMod)

  def clicked: Observable[Event] = {
    DOM.on(id, "click")
  }

}

object DOM extends ReactEventAliases {

  private def eventListener(target: EventTarget, eventType: String): Observable[Event] =
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
        case target: HTMLElement => target.id == id.toString()
        case _ => false
      }
    })
  }

  def valueAttribute[T](value: T): TagMod = {
    Seq(
      ^.value := value.toString(),
      ^.onChange ==> ((s: CompState.Access[T]) => s.modState(x => x)) // XXX very ugly workaround for https://github.com/facebook/react/issues/1118
    )
  }

  def checkedAttribute(value: Boolean): TagMod = {
    Seq(
      ^.checked := value,
      ^.onChange ==> ((s: CompState.Access[Boolean]) => s.modState(x => x)) // XXX very ugly workaround for https://github.com/facebook/react/issues/1118
    )
  }

}
