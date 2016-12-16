package hu.thsoft.spiral.editor

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.{ChoiceData, CurrentCase, Data}
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class ChoiceEditor[Choice <: Data](data: ChoiceData[Choice], id: Id) extends Component {

  type State = Stored[CurrentCase[Choice]]

  def state = data.caseChanged

  def output(state: State) = {
    state.fold(
      invalid =>
        Output(
          view = Observable.pure(GenericEditor.viewInvalid(invalid)),
          reaction = Observable.empty
        ),
      selectedCase => {
        val valueEditor = GenericEditor(selectedCase.choice, id.child("value"))
        val choices = data.cases.map(dataCase => Choice(dataCase.name, dataCase.name))
        val cases = new ChoiceList(id.child("case"), choices, selectedCase.name)()
        val view: Observable[ReactElement] =
          valueEditor.viewChanged.map(valueView => {
            <.div(
              <.label(
                "Case:",
                cases.view
              ),
              valueView
            )
          })
        val caseChanged = cases.changed.map(data.setCase(_))
        val reaction =
          Observable.merge(
            caseChanged,
            valueEditor.reacted
          )
        Output(view, reaction)
      }
    )
  }

}