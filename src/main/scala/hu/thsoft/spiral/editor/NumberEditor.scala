package hu.thsoft.spiral.editor

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.NumberData
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class NumberEditor(data: NumberData, id: Id) extends Component {

  type State = Stored[Double]

  def state = data.changed

  def output(state: State) = {
    val value: Double = state.right.getOrElse(0)
    val input = new NumberInput(id, value)(^.min := data.min, ^.max := data.max)
    Output(
      view = Observable.pure(input.view),
      reaction = input.changed.map(data.set(_))
    )
  }

}