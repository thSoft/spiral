package hu.thsoft.spiral.examples.generic

import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral._
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