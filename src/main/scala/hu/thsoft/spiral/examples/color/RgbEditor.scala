package hu.thsoft.spiral.examples.color

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral._
import hu.thsoft.spiral.examples.generic.{ExampleUtils, NumberEditor}
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

class RgbEditor(data: RgbData, id: Id) extends Component {

  type State = Rgb

  def state = {
    Observable.combineLatestMap3(
      data.red.changed, data.green.changed, data.blue.changed
    )(
      (red, green, blue) => Rgb(red, green, blue)
    )
  }

  def output(state: State) = {
    val min = 0
    val max = 255

    val redEditor = new NumberEditor(data.red, id.child("red"), min, max)
    val greenEditor = new NumberEditor(data.green, id.child("green"), min, max)
    val blueEditor = new NumberEditor(data.blue, id.child("blue"), min, max)

    val color =
      for (
        redValue <- state.red.right;
        greenValue <- state.green.right;
        blueValue <- state.blue.right
      ) yield ExampleUtils.viewColor(redValue, greenValue, blueValue)
    val view: Observable[ReactElement] =
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
    val reaction =
      Observable.merge(
        redEditor.reacted,
        greenEditor.reacted,
        blueEditor.reacted
      )
    Output(view, reaction)
  }

}