package hu.thsoft.spiral.examples

import hu.thsoft.spiral.Clickable
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data
import hu.thsoft.spiral.ObservableUtils
import hu.thsoft.spiral.Output
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable
import japgolly.scalajs.react.ReactElement
import hu.thsoft.spiral.Id

class DeletableComponent[ComponentData <: Data](data: ComponentData, id: Id)(makeComponent: (ComponentData, Id) => Component) extends Component {

  type State = Unit

  def state = ObservableUtils.constant(())

  def output(state: State) = {
    val component = makeComponent(data, id)
    val delete = new Clickable(<.button(_))(id.child("delete"))("Delete")
    val view: Observable[ReactElement] =
      component.viewChanged.map(componentView =>
        <.span(
          componentView,
          delete.view
        )
      )
    val reaction =
      Observable.merge(
        component.reacted,
        delete.clicked.map(_ => data.delete)
      )
    Output(view, reaction)
  }

}