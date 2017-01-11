package hu.thsoft.spiral.editor

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.BooleanData
import hu.thsoft.spiral.data.Data.Stored
import monix.reactive.Observable

class BooleanEditor(data: BooleanData, id: Id) extends Component {

  type State = Stored[Boolean]

  def state = data.changed

  def output(state: State) = {
    val value = state.right.getOrElse(false)
    val checkbox = new Checkbox(id, value)()
    Output(
      view = Observable.pure(checkbox.view),
      reaction = checkbox.changed.map(data.set(_))
    )
  }

}