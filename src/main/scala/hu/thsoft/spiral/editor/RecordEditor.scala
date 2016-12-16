package hu.thsoft.spiral.editor

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.RecordData
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._

case class RecordEditorField(name: String, component: Component)

class RecordEditor(data: RecordData, id: Id) extends Component {

  type State = Seq[RecordEditorField]

  def state = {
    ObservableUtils.constant(
      data.fields.map(field => RecordEditorField(field.name, GenericEditor(field.data, id.child(field.name))))
    )
  }

  def output(state: State) = {
    val views = state.map(_.component.viewChanged)
    val fieldNames = state.map(_.name)
    val view = ObservableUtils.combineLatestList(views).map[ReactElement](currentViews => {
      val fields = fieldNames.zip(currentViews).map { case (fieldName, currentView) => <.label(s"$fieldName:", currentView) }
      <.fieldset(<.legend(data.recordName), fields)
    })
    val reactions = state.map(_.component.reacted)
    val reaction = ObservableUtils.merge(reactions)
    Output(view, reaction)
  }

}