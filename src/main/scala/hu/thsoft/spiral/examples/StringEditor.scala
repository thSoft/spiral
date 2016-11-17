package hu.thsoft.spiral.examples

import org.scalajs.dom._

import hu.thsoft.spiral.Component
import hu.thsoft.spiral.DOM
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Id._
import hu.thsoft.spiral.StringData
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class StringEditor(data: StringData, parentId: Id) extends Component[Stored[String]] {

  def state = data.changed

  val inputId = parentId.child("input")

  def view(state: Stored[String]) = {
    Observable.pure(
      state.fold(
        invalid =>
          ExampleUtils.viewInvalid(invalid),
        value =>
          <.input(
            ^.id := inputId.toString(),
            ExampleUtils.valueAttribute(value)
          )
      )
    )
  }

  def react(state: Stored[String]) = {
    DOM.inputChanged(inputId).map(value => {
      data.set(value)
    })
  }

}