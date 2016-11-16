package hu.thsoft.spiral.examples

import scala.scalajs.js.JSApp

import org.scalajs.dom._

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Case
import hu.thsoft.spiral.ChoiceData
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.DOM
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

sealed trait Color
case class RgbColor(rgbData: RgbData) extends Color
case class CmykColor(cmykData: CmykData) extends Color

class ColorData(firebase: Firebase) extends ChoiceData[Color](firebase) {

  def cases = Seq(
    new Case(ColorData.caseNameRgb, (firebase: Firebase) => RgbColor(new RgbData(firebase))),
    new Case(ColorData.caseNameCmyk, (firebase: Firebase) => CmykColor(new CmykData(firebase)))
  )

}

object ColorData {
  val caseNameRgb = "rgb"
  val caseNameCmyk = "cmyk"
}

class ColorEditor(data: ColorData, parentId: Id) extends Component[Stored[Color]] {

  def state = {
    data.caseChanged
  }

  val caseId = parentId.child("case")
  val valueId = parentId.child("value")

  def view(state: Stored[Color]) = {
    state.fold(
      invalid => {
        Observable.pure(ExampleUtils.viewInvalid(invalid))
      },
      color => {
        val caseName =
          color match {
            case RgbColor(_) => ColorData.caseNameRgb
            case CmykColor(_) => ColorData.caseNameCmyk
          }
        val valueViewChanged =
          color match {
            case RgbColor(rgbData) => {
              new RgbEditor(rgbData, valueId).viewChanged
            }
            case CmykColor(cmykData) => {
              new CmykEditor(cmykData, valueId).viewChanged
            }
          }
        valueViewChanged.map(valueView => {
          <.div(
            <.select(
              ^.id := caseId.toString(),
              ExampleUtils.valueAttribute(caseName),
              <.option(
                ^.value := ColorData.caseNameRgb,
                "RGB"
              ),
              <.option(
                ^.value := ColorData.caseNameCmyk,
                "CMYK"
              )
            ),
            valueView
          )
        })
      }
    )
  }

  def react(state: Stored[Color]) = {
    val caseChanged =
      DOM.selectChanged(caseId).map(caseName => data.setCase(caseName))
    val valueEditorReacted =
      state.fold(
        invalid => {
          Observable.empty
        },
        color => {
          color match {
            case RgbColor(rgbData) => {
              new RgbEditor(rgbData, valueId).reacted
            }
            case CmykColor(cmykData) => {
              new CmykEditor(cmykData, valueId).reacted
            }
          }
        }
      )
    Observable.merge(
      caseChanged,
      valueEditorReacted
    )
  }

}

object ColorEditorApp extends JSApp {

  def main() {
    val container = document.createElement("div")
    document.body.appendChild(container)
    val component = new ColorEditor(new ColorData(new Firebase("https://thsoft.firebaseio.com/spiral/examples/color")), Id.root)
    Component.run(component, container)
  }

}