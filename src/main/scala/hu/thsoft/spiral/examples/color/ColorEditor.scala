package hu.thsoft.spiral.examples.color

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.{Case, ChoiceData, DataStore}
import hu.thsoft.spiral.examples.generic.ExampleUtils
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

sealed trait Color
case class RgbColor(rgbData: RgbData) extends Color
case class CmykColor(cmykData: CmykData) extends Color

class ColorData(dataStore: DataStore) extends ChoiceData[Color](dataStore) {

  def cases = Seq(
    new Case(ColorData.caseNameRgb, (dataStore: DataStore) => RgbColor(new RgbData(dataStore))),
    new Case(ColorData.caseNameCmyk, (dataStore: DataStore) => CmykColor(new CmykData(dataStore)))
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
              <.label(
                "Color model:",
                cases.view
              ),
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