package hu.thsoft.spiral.examples.todolist

import hu.thsoft.spiral.Choice
import hu.thsoft.spiral.ChoiceList

import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.ListData
import hu.thsoft.spiral.ObservableUtils
import hu.thsoft.spiral.Output
import hu.thsoft.spiral.ReferenceData
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

case class TodoReference(
  selected: Stored[TodoData],
  available: List[TodoData]
)

case class Remote[D <: Data, T](data: D, value: T)

class TodoReferenceEditor(data: ReferenceData[TodoData], id: Id, availableTodos: ListData[TodoData], todoToOmit: TodoData) extends Component {

  type State = TodoReference

  def state = {
    Observable.combineLatestMap2(
      data.referredChanged, availableTodos.changed
    )(
      (selected, available) => TodoReference(selected, available.filter(_ != todoToOmit))
    )
  }

  def output(state: State) = {
    def makeRemoteName(todoData: TodoData): Observable[Remote[TodoData, Stored[String]]] = {
      todoData.name.changed.map(Remote(todoData, _))
    }
    val choiceList: Observable[ChoiceList[Option[TodoData]]] =
      ObservableUtils.combineLatestList(state.available.map(makeRemoteName)).map(remoteNames => {
        val noneChoice: Choice[Option[TodoData]] = Choice(None, "(none)")
        val choices = noneChoice +: remoteNames.map(remoteName => {
          val item: Option[TodoData] = Some(remoteName.data)
          Choice(item, remoteName.value.right.toOption.getOrElse("(can't determine name)"))
        })
        val selectedItem = state.selected.right.toOption
        new ChoiceList(id, choices, selectedItem)()
      })
    val view = choiceList.map(_.view)
    val reaction =
      choiceList.switchMap(_.changed).map(selectedTodoData => {
        selectedTodoData.fold(data.delete)(data.setReferred(_))
      })
    Output(view, reaction)
  }

}