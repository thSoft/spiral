package hu.thsoft.spiral.examples

import org.scalajs.dom._

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Component

import hu.thsoft.spiral.Invalid
import hu.thsoft.spiral.ListData
import japgolly.scalajs.react.CompState
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._

object ExampleUtils {

  def viewInvalid(invalid: Invalid): ReactElement = {
    <.a(
      ^.href := invalid.firebase.toString(),
      s"Error, expected ${invalid.expectedTypeName}"
    )
  }

  def viewColor(red: Double, green: Double, blue: Double): ReactElement = {
    <.div(^.backgroundColor := s"rgb(${red.toInt}, ${green.toInt}, ${blue.toInt})", "\u00a0")
  }

  def valueAttribute[T](value: T): TagMod = {
    Seq(
      ^.value := value.toString(),
      ^.onChange ==> ((s: CompState.Access[T]) => s.modState(x => x)) // XXX very ugly workaround for https://github.com/facebook/react/issues/1118
    )
  }

  def checkedAttribute(value: Boolean): TagMod = {
    Seq(
      ^.checked := value,
      ^.onChange ==> ((s: CompState.Access[Boolean]) => s.modState(x => x)) // XXX very ugly workaround for https://github.com/facebook/react/issues/1118
    )
  }

  def runComponent(component: Component[_]) {
    val container = document.createElement("div")
    document.body.appendChild(container)
    Component.run(component, container)
  }

  def todoListData = new ListData(new Firebase("https://thsoft.firebaseio.com/spiral/examples/todoList"))(new TodoData(_))

}