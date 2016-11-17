package hu.thsoft.spiral.examples

import scala.scalajs.js.JSApp

import hu.thsoft.spiral.Action
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.DOM
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.ListData
import hu.thsoft.spiral.ObservableUtils
import japgolly.scalajs.react.vdom.prefix_<^._

class TodoListEditor(data: ListData[TodoData], parentId: Id) extends Component[List[TodoData]] {

  def state = data.changed

  def editors(state: List[TodoData]): List[Component[_]] = {
    state.map(todoData => {
      new DeletableComponent(todoData, parentId.child(todoData.firebase.toString))(new TodoEditor(_, _, data))
    })
  }

  val adderId = parentId.child("_add")

  def view(state: List[TodoData]) = {
    ObservableUtils.combineLatestList(editors(state).map(_.viewChanged)).map(todoViews => {
      <.div(
        todoViews.map(<.div(_)),
        <.button(
          ^.id := adderId.toString(),
          "Add"
        )
      )
    })
  }

  def react(state: List[TodoData]) = {
    val todoReactions = editors(state).map(_.reacted)
    val add = DOM.clicked(adderId).map(_ => {
      data.add(todoData => {
        Action.sequential(Seq(todoData.name.set(""), todoData.completed.set(false)))
      })
    })
    ObservableUtils.merge(todoReactions :+ add)
  }

}

object TodoListEditorApp extends JSApp {

  def main() {
    ExampleUtils.runComponent(new TodoListEditor(ExampleUtils.todoListData, Id.root))
  }

}