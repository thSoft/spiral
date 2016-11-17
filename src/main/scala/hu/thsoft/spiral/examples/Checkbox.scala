package hu.thsoft.spiral.examples

import org.scalajs.dom._

import hu.thsoft.spiral.BooleanData
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.DOM
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Id._
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class Checkbox(data: BooleanData, parentId: Id) extends Component[Stored[Boolean]] {

  def state = data.changed

  val inputId = parentId.child("input")

  def view(state: Stored[Boolean]) = {
    Observable.pure(
      state.fold(
        invalid =>
          ExampleUtils.viewInvalid(invalid),
        value =>
          <.input.checkbox(
            ^.id := inputId.toString(),
            ExampleUtils.checkedAttribute(value)
          )
      )
    )
  }

  def react(state: Stored[Boolean]) = {
    DOM.checkableChanged(inputId).map(value => {
      data.set(value)
    })
  }

}