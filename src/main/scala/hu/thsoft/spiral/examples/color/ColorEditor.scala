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
object RgbColor { val caseName = "rgb" }
case class CmykColor(cmykData: CmykData) extends Color
object CmykColor { val caseName = "cmyk" }

class ColorData(dataStore: DataStore) extends ChoiceData[Color](dataStore) {

  def cases = Seq(
    Case(RgbColor.caseName, dataStore => RgbColor(new RgbData(dataStore))),
    Case(CmykColor.caseName, dataStore => CmykColor(new CmykData(dataStore)))
  )

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
        val valueEditor =
          color match {
            case RgbColor(rgbData) => new RgbEditor(rgbData, valueId)
            case CmykColor(cmykData) => new CmykEditor(cmykData, valueId)
          }
        val choices = Seq(
          Choice(RgbColor.caseName, "RGB"),
          Choice(CmykColor.caseName, "CMYK")
        )
        val selectedCaseName =
          color match {
            case RgbColor(_) => RgbColor.caseName
            case CmykColor(_) => CmykColor.caseName
          }
        val cases = new ChoiceList(id.child("case"), choices, selectedCaseName)()
        val view: Observable[ReactElement] =
          valueEditor.viewChanged.map(valueView => {
            <.div(
              <.label(
                "Color model:",
                cases.view
              ),
              valueView
            )
          })
        val caseChanged = cases.changed.map(data.setCase(_))
        val reaction =
          Observable.merge(
            caseChanged,
            valueEditor.reacted
          )
        Output(view, reaction)
      }
    )
  }

}