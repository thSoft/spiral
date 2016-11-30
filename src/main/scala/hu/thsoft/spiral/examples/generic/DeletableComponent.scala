package hu.thsoft.spiral.examples.generic

import hu.thsoft.spiral._
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

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