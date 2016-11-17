package hu.thsoft.spiral.examples

import scala.scalajs.js.JSApp

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.BooleanData
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.RecordData
import hu.thsoft.spiral.ReferenceData
import hu.thsoft.spiral.StringData
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable
import hu.thsoft.spiral.ListData
import hu.thsoft.spiral.DOM
import hu.thsoft.spiral.ObservableUtils
import hu.thsoft.spiral.Data

case class TodoReference(
  selected: Stored[TodoData],
  available: List[TodoData]
)

case class Remote[T](data: Data, value: T)

class TodoReferenceEditor(data: ReferenceData[TodoData], parentId: Id, availableTodos: ListData[TodoData], todoToOmit: TodoData) extends Component[TodoReference] {

  def state = {
    Observable.combineLatestMap2(
      data.referredChanged, availableTodos.changed
    )(
      (selected, available) => TodoReference(selected, available.filter(!_.isSame(todoToOmit)))
    )
  }

  val selectId = parentId.child("select")

  val noneValue = "none"

  def view(state: TodoReference) = {
    def makeRemoteName(todoData: TodoData): Observable[Remote[Stored[String]]] = {
      todoData.name.changed.map(Remote(todoData, _))
    }
    def optionAttributes(value: String): TagMod = {
      Seq(
        ^.value := value,
        ^.selected := state.selected.right.toOption.map(_.firebase.toString() == value)
      )
    }

    ObservableUtils.combineLatestList(state.available.map(makeRemoteName)).map(remoteNames =>
      <.select(
        ^.id := selectId.toString(),
        <.option(
          optionAttributes(noneValue),
          "(none)"
        ),
        remoteNames.flatMap(remoteName => {
          remoteName.value.right.toOption.map(name =>
            <.option(
              optionAttributes(remoteName.data.firebase.toString()),
              name
            )
          )
        })
      )
    )
  }

  def react(state: TodoReference) = {
    DOM.selectChanged(selectId).map(value => {
      if (value == noneValue) {
        data.delete
      } else {
        data.setReferred(new TodoData(new Firebase(value)))
      }
    })
  }

}