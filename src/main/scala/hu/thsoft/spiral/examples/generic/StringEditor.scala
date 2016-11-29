package hu.thsoft.spiral.examples.generic

import org.scalajs.dom._

import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Output
import hu.thsoft.spiral.StringData
import hu.thsoft.spiral.TextInput
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class StringEditor(data: StringData, id: Id) extends Component {

  type State = Stored[String]

  def state = data.changed

  def output(state: State) = {
    state.fold(
      invalid =>
        Output(
          view = Observable.pure(ExampleUtils.viewInvalid(invalid)),
          reaction = Observable.empty
        ),
      value => {
        val input = new TextInput(id, value)()
        Output(
          view = Observable.pure(input.view),
          reaction = input.changed.map(data.set(_))
        )
      }
    )
  }

}