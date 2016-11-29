package hu.thsoft.spiral.examples.todolist

import org.scalajs.dom._

import hu.thsoft.firebase.Firebase

import hu.thsoft.spiral.Action
import hu.thsoft.spiral.Clickable
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.ListData
import hu.thsoft.spiral.ObservableUtils
import hu.thsoft.spiral.Output
import hu.thsoft.spiral.examples.generic.DeletableComponent
import hu.thsoft.spiral.examples.generic.ExampleUtils
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class TodoListEditor(data: ListData[TodoData], id: Id) extends Component {

  type State = List[TodoData]

  def state = data.changed

  def output(state: State) = {
    val editors =
      state.map(todoData => {
        new DeletableComponent(todoData, id.child(todoData.firebase.toString()))(new TodoEditor(_, _, data))
      })
    val add = new Clickable(<.button(_))(id.child("add"))("Add")
    val view: Observable[ReactElement] =
      ObservableUtils.combineLatestList(editors.map(_.viewChanged)).map(todoViews => {
        <.div(
          todoViews.map(<.div(_)),
          add.view
        )
      })
    val todoReactions = editors.map(_.reacted)
    val addClicked = add.clicked.map(_ => {
      data.add(todoData => {
        Action.sequential(Seq(todoData.name.set(""), todoData.completed.set(false)))
      })
    })
    val reaction =
      ObservableUtils.merge(todoReactions :+ addClicked)
    Output(view, reaction)
  }

}

object TodoListEditorApp {

  def main() {
    def todoListData = new ListData(new Firebase("https://thsoft.firebaseio.com/spiral/examples/todoList"))(new TodoData(_))
    ExampleUtils.runComponent(new TodoListEditor(todoListData, Id.root))
  }

}