package hu.thsoft.spiral.examples.todolist

import hu.thsoft.spiral._
import hu.thsoft.spiral.data._
import hu.thsoft.spiral.examples.generic.{BooleanEditor, StringEditor}
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class TodoData(dataStore: DataStore) extends RecordData(dataStore) {

  val name = field("name", new StringData(_))

  val completed = field("completed", new BooleanData(_))

  val blockedBy = field("blockedBy", new ReferenceData(_)(new TodoData(_)))

}

class TodoEditor(val data: TodoData, id: Id, availableTodos: ListData[TodoData]) extends Component {

  type State = Unit

  def state = ObservableUtils.constant(())

  def output(state: State) = {
    val nameEditor = new StringEditor(data.name, id.child("name"))
    val completedEditor = new BooleanEditor(data.completed, id.child("completed"))
    val blockedByEditor = new TodoReferenceEditor(data.blockedBy, id.child("blockedBy"), availableTodos, data)
    val view: Observable[ReactElement] =
      Observable.combineLatestMap3(
        nameEditor.viewChanged, completedEditor.viewChanged, blockedByEditor.viewChanged
      )(
       (nameView, completedView, blockedByView) =>
          <.span(
            completedView,
            nameView,
            "Blocked by:", blockedByView
          )
      )
    val reaction =
      Observable.merge(
        nameEditor.reacted,
        completedEditor.reacted,
        blockedByEditor.reacted
      )
    Output(view, reaction)
  }

}