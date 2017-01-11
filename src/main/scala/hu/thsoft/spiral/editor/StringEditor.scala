package hu.thsoft.spiral.editor

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.StringData
import monix.reactive.Observable

class StringEditor(data: StringData, id: Id) extends Component {

  type State = Stored[String]

  def state = data.changed

  def output(state: State) = {
    val value = state.right.getOrElse("")
    val input = new TextInput(id, value)()
    Output(
      view = Observable.pure(input.view),
      reaction = input.changed.map(data.set(_))
    )
  }

}