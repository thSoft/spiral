package hu.thsoft.spiral.examples.todolist

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.BooleanData
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.ListData
import hu.thsoft.spiral.ObservableUtils
import hu.thsoft.spiral.Output
import hu.thsoft.spiral.RecordData
import hu.thsoft.spiral.ReferenceData
import hu.thsoft.spiral.StringData
import hu.thsoft.spiral.examples.generic.BooleanEditor
import hu.thsoft.spiral.examples.generic.StringEditor
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class TodoData(firebase: Firebase) extends RecordData(firebase) {

  val name = newField("name", new StringData(_))

  val completed = newField("completed", new BooleanData(_))

  val blockedBy = newField("blockedBy", new ReferenceData(_)(new TodoData(_)))

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