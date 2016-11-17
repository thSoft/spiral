package hu.thsoft.spiral.examples

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.NumberData
import hu.thsoft.spiral.RecordData
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class RgbData(firebase: Firebase) extends RecordData(firebase) {

  val red = newField("red", new NumberData(_))

  val green = newField("green", new NumberData(_))

  val blue = newField("blue", new NumberData(_))

}

case class Rgb(
  red: Stored[Double],
  green: Stored[Double],
  blue: Stored[Double]
)

class RgbEditor(data: RgbData, parentId: Id) extends Component[Rgb] {

  def state = {
    Observable.combineLatestMap3(
      data.red.changed, data.green.changed, data.blue.changed
    )(
      (red, green, blue) => Rgb(red, green, blue)
    )
  }

  val min = 0
  val max = 255

  val redEditor = new NumberEditor(data.red, parentId.child("red"), min, max)
  val greenEditor = new NumberEditor(data.green, parentId.child("green"), min, max)
  val blueEditor = new NumberEditor(data.blue, parentId.child("blue"), min, max)

  def view(state: Rgb) = {
    val color =
      for (
        redValue <- state.red.right;
        greenValue <- state.green.right;
        blueValue <- state.blue.right
      ) yield ExampleUtils.viewColor(redValue, greenValue, blueValue)
    Observable.combineLatestMap3(
      redEditor.viewChanged, greenEditor.viewChanged, blueEditor.viewChanged
    )(
     (redView, greenView, blueView) =>
        <.div(
          <.table(<.tbody(
            <.tr(<.td("Red: "), <.td(redView)),
            <.tr(<.td("Green: "), <.td(greenView)),
            <.tr(<.td("Blue: "), <.td(blueView))
          )),
          color.right.getOrElse[ReactElement](<.span("Error"))
        )
    )
  }

  def react(state: Rgb) = {
    Observable.merge(
      redEditor.reacted,
      greenEditor.reacted,
      blueEditor.reacted
    )
  }

}