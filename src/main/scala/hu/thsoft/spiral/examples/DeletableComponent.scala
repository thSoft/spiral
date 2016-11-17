package hu.thsoft.spiral.examples

import hu.thsoft.spiral.Component
import hu.thsoft.spiral.DOM
import hu.thsoft.spiral.Data

import hu.thsoft.spiral.Id
import hu.thsoft.spiral.ObservableUtils
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class DeletableComponent[ComponentData <: Data](data: ComponentData, parentId: Id)(makeComponent: (ComponentData, Id) => Component[_]) extends Component[Unit] {

  def state = ObservableUtils.constant(())

  val component = makeComponent(data, parentId)

  val deleteId = parentId.child("delete")

  def view(state: Unit) = {
    component.viewChanged.map(componentView =>
      <.span(
        componentView,
        <.button(
          ^.id := deleteId.toString(),
          "Delete"
        )
      )
    )
  }

  def react(state: Unit) = {
    Observable.merge(
      component.reacted,
      DOM.clicked(deleteId).map(_ => data.delete)
    )
  }

}