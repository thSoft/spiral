package hu.thsoft.spiral.examples

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Component.Action
import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.{FirebaseDataStore, LocalDataStore, StringData}
import hu.thsoft.spiral.examples.color.{Color, ColorData}
import hu.thsoft.spiral.examples.generic.{ExampleUtils, GenericEditor}
import hu.thsoft.spiral.examples.misc.{CatGifs, Clock}
import hu.thsoft.spiral.examples.todolist.TodoList
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

import scala.scalajs.js.JSApp

case class Example(name: String, component: Component)

class ExampleSelector(data: StringData, id: Id) extends Component {

  type State = Stored[String]

  def state = data.changed

  def output(state: State) = {
    val selectedExampleName = state.right.getOrElse("")
    val exampleId = id.child("example")
    val examples = Seq(
      Example("Cat GIFs", new CatGifs(new StringData(new LocalDataStore(Id.fromString("catGifUrl"))), exampleId)),
      Example("Clock", new Clock),
      Example("Color", GenericEditor(Color.data, exampleId)),
      Example("Todo List", GenericEditor(TodoList.data, exampleId))
    )
    val none = new Choice(Option.empty[Example], "(none)")
    val exampleChoices = examples.map(example => new Choice(Option(example), example.name))
    val choices = none +: exampleChoices
    val selectedExample = examples.find(_.name == selectedExampleName)
    val exampleList = new ChoiceList(id.child("name"), choices, selectedExample)()
    val exampleListView: ReactElement =
      <.label(
        "Example:",
        exampleList.view
      )
    val view: Observable[ReactElement] =
      selectedExample.fold(Observable.pure(exampleListView))(_.component.viewChanged.map(exampleView => {
        <.div(
          exampleListView,
          <.hr,
          exampleView
        )
      }))
    val exampleChanged = exampleList.changed.map(_.fold(data.delete)(example => data.set(example.name)))
    val exampleReacted = selectedExample.fold(Observable.empty[Action])(_.component.reacted)
    val reaction =
      Observable.merge(
        exampleChanged,
        exampleReacted
      )
    Output(view, reaction)
  }

}

object ExamplesApp extends JSApp {

  def main() {
    ExampleUtils.runComponent(new ExampleSelector(new StringData(new LocalDataStore(Id.fromString("example"))), Id.root))
  }

}