package hu.thsoft.spiral.examples

import scala.scalajs.js.JSApp

import hu.thsoft.spiral.Action
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.DOM
import hu.thsoft.spiral.ListData
import hu.thsoft.spiral.ObservableUtils
import japgolly.scalajs.react.vdom.prefix_<^._
import hu.thsoft.spiral.Clickable
import hu.thsoft.spiral.Output
import japgolly.scalajs.react.ReactElement
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import japgolly.scalajs.react.ReactDOM
import monix.execution.Scheduler.Implicits.global
import hu.thsoft.spiral.Id

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

object TodoListEditorApp extends JSApp {

  def main() {
    ExampleUtils.runComponent(new TodoListEditor(ExampleUtils.todoListData, Id.root))
  }

}