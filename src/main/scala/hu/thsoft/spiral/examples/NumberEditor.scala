package hu.thsoft.spiral.examples

import scala.scalajs.js.JSApp

import org.scalajs.dom._

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.AtomicData
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Id._
import hu.thsoft.spiral.Invalid
import hu.thsoft.spiral.NumberData
import japgolly.scalajs.react.CompState
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.ReactEventAliases
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable
import hu.thsoft.spiral.DOM

class NumberEditor(data: NumberData, parentId: Id, min: Double, max: Double) extends Component[Stored[Double]] {

  def state = data.changed

  val inputId = parentId.child("input")

  def view(state: Stored[Double]) = {
    Observable.pure(
      state.fold(
        invalid =>
          ExampleUtils.viewInvalid(invalid),
        value =>
          <.input.number(
            ^.id := inputId.toString(),
            ^.min := min,
            ^.max := max,
            ExampleUtils.valueAttribute(value)
          )
      )
    )
  }

  def react(state: Stored[Double]) = {
    DOM.numericInputChanged(inputId).map(value => {
      data.set(value)
    })
  }

}