package hu.thsoft.spiral.examples.generic

import org.scalajs.dom._

import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Invalid
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

  def runComponent(component: Component) {
    val container = document.createElement("div")
    document.body.appendChild(container)
    Component.run(component, container)
  }

}