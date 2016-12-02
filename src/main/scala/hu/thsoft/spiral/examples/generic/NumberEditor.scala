package hu.thsoft.spiral.examples.generic

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.NumberData
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class NumberEditor(data: NumberData, id: Id, min: Double, max: Double) extends Component {

  type State = Stored[Double]

  def state = data.changed

  def output(state: State) = {
    state.fold(
      invalid =>
        Output(
          view = Observable.pure(ExampleUtils.viewInvalid(invalid)),
          reaction = Observable.empty
        ),
      value => {
        val input = new NumberInput(id, value)(^.min := min, ^.max := max)
        Output(
          view = Observable.pure(input.view),
          reaction = input.changed.map(data.set(_))
        )
      }
    )
  }

}