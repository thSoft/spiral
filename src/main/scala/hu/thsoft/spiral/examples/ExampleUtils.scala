package hu.thsoft.spiral.examples

import hu.thsoft.spiral.Invalid
import japgolly.scalajs.react.CompState
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.ReactEventAliases
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

}