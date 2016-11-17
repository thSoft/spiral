package hu.thsoft.spiral.examples

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.BooleanData
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.ListData
import hu.thsoft.spiral.ObservableUtils
import hu.thsoft.spiral.RecordData
import hu.thsoft.spiral.ReferenceData
import hu.thsoft.spiral.StringData
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class TodoData(firebase: Firebase) extends RecordData(firebase) {

  val name = newField("name", new StringData(_))

  val completed = newField("completed", new BooleanData(_))

  val blockedBy = newField("blockedBy", new ReferenceData(_)(new TodoData(_)))

}

class TodoEditor(val data: TodoData, parentId: Id, availableTodos: ListData[TodoData]) extends Component[Unit] {

  def state = ObservableUtils.constant(())

  val nameEditor = new StringEditor(data.name, parentId.child("name"))
  val completedEditor = new Checkbox(data.completed, parentId.child("completed"))
  val blockedByEditor = new TodoReferenceEditor(data.blockedBy, parentId.child("blockedBy"), availableTodos, data)

  def view(state: Unit) = {
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
  }

  def react(state: Unit) = {
    Observable.merge(
      nameEditor.reacted,
      completedEditor.reacted,
      blockedByEditor.reacted
    )
  }

}