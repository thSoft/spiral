package hu.thsoft.spiral.examples.color

import org.scalajs.dom._

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Case
import hu.thsoft.spiral.Choice
import hu.thsoft.spiral.ChoiceData
import hu.thsoft.spiral.ChoiceList
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Output
import hu.thsoft.spiral.examples.generic.ExampleUtils
import japgolly.scalajs.react.ReactElement
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

class ColorEditor(data: ColorData, id: Id) extends Component {

  type State = Stored[Color]

  def state = data.caseChanged

  def output(state: State) = {
    state.fold(
      invalid =>
        Output(
          view = Observable.pure(ExampleUtils.viewInvalid(invalid)),
          reaction = Observable.empty
        ),
      color => {
        val valueId = id.child("value")
        val valueViewChanged =
          color match {
            case RgbColor(rgbData) => new RgbEditor(rgbData, valueId).viewChanged
            case CmykColor(cmykData) => new CmykEditor(cmykData, valueId).viewChanged
          }
        val choices = Seq(
          Choice(ColorData.caseNameRgb, "RGB"),
          Choice(ColorData.caseNameCmyk, "CMYK")
        )
        val selectedCaseName =
          color match {
            case RgbColor(_) => ColorData.caseNameRgb
            case CmykColor(_) => ColorData.caseNameCmyk
          }
        val cases = new ChoiceList(id.child("case"), choices, selectedCaseName)()
        val view: Observable[ReactElement] =
          valueViewChanged.map(valueView => {
            <.div(
              cases.view,
              valueView
            )
          })
        val caseChanged = cases.changed.map(data.setCase(_))
        val valueEditorReacted =
          color match {
            case RgbColor(rgbData) => new RgbEditor(rgbData, valueId).reacted
            case CmykColor(cmykData) => new CmykEditor(cmykData, valueId).reacted
          }
        val reaction =
          Observable.merge(
            caseChanged,
            valueEditorReacted
          )
        Output(view, reaction)
      }
    )
  }

}

object ColorEditorApp {

  def main() {
    ExampleUtils.runComponent(new ColorEditor(new ColorData(new Firebase("https://thsoft.firebaseio.com/spiral/examples/color")), Id.root))
  }

}