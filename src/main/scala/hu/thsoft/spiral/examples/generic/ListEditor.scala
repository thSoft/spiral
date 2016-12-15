package hu.thsoft.spiral.examples.generic

import hu.thsoft.spiral.Component.Action
import hu.thsoft.spiral._
import hu.thsoft.spiral.data._
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class ListEditor[Element <: Data](data: ListData[Element], id: Id) extends Component {

  type State = List[Element]

  def state = data.changed

  def output(state: State) = {
    val editors =
      state.map(elementData => {
        new DeletableComponent(elementData, id.child(elementData.dataStore.url))(GenericEditor(_, _))
      })
    val add = new Clickable(<.button(_))(id.child("add"))("Add")
    val view: Observable[ReactElement] =
      ObservableUtils.combineLatestList(editors.map(_.viewChanged)).map(elementViews => {
        <.div(
          elementViews.map(<.div(_)),
          add.view
        )
      })
    val elementReactions = editors.map(_.reacted)
    val addClicked = add.clicked.map(_ => {
      data.add(elementData => {
        setDefaultValue(elementData)
      })
    })
    val reaction = ObservableUtils.merge(elementReactions :+ addClicked)
    Output(view, reaction)
  }

  def setDefaultValue(elementData: Data): Action = {
    elementData match {
      case elementData: BooleanData => elementData.set(false)
      case elementData: NumberData => elementData.set(0)
      case elementData: StringData => elementData.set("")
      case elementData: RecordData => Action.sequential(elementData.fields.map(field => setDefaultValue(field.data)))
      case elementData: ChoiceData[_] => {
        val firstCase = elementData.cases.head
        val setCase = elementData.setCase(firstCase.name)
        val setValue = setDefaultValue(elementData.getValueData(firstCase))
        Action.sequential(Seq(setCase, setValue))
      }
      case _: ListData[_] => Action.nop
      case _: ReferenceData[_] => Action.nop
    }
  }

}

class DeletableComponent[ComponentData <: Data](data: ComponentData, id: Id)(makeComponent: (ComponentData, Id) => Component) extends Component {

  type State = Unit

  def state = ObservableUtils.constant(())

  def output(state: State) = {
    val component = makeComponent(data, id)
    val delete = new Clickable(<.button(_))(id.child("delete"))("Delete")
    val view: Observable[ReactElement] =
      component.viewChanged.map(componentView =>
        <.span(
          componentView,
          delete.view
        )
      )
    val reaction =
      Observable.merge(
        component.reacted,
        delete.clicked.map(_ => data.delete)
      )
    Output(view, reaction)
  }

}