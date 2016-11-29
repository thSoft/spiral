package hu.thsoft.spiral.examples.generic

import org.scalajs.dom._

import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.NumberData
import hu.thsoft.spiral.NumberInput
import hu.thsoft.spiral.Output
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