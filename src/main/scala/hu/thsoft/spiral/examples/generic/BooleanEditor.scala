package hu.thsoft.spiral.examples.generic

import org.scalajs.dom._

import hu.thsoft.spiral.BooleanData
import hu.thsoft.spiral.Checkbox
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Output
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class BooleanEditor(data: BooleanData, id: Id) extends Component {

  type State = Stored[Boolean]

  def state = data.changed

  def output(state: State) = {
    state.fold(
      invalid =>
        Output(
          view = Observable.pure(ExampleUtils.viewInvalid(invalid)),
          reaction = Observable.empty
        ),
      value => {
        val checkbox = new Checkbox(id, value)()
        Output(
          view = Observable.pure(checkbox.view),
          reaction = checkbox.changed.map(data.set(_))
        )
      }
    )
  }

}