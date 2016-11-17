package hu.thsoft.spiral.examples

import org.scalajs.dom._

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.NumberData
import hu.thsoft.spiral.RecordData
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable

class CmykData(firebase: Firebase) extends RecordData(firebase) {

  val cyan = newField("cyan", new NumberData(_))

  val magenta = newField("magenta", new NumberData(_))

  val yellow = newField("yellow", new NumberData(_))

  val black = newField("black", new NumberData(_))

}

case class Cmyk(
  cyan: Stored[Double],
  magenta: Stored[Double],
  yellow: Stored[Double],
  black: Stored[Double]
)

class CmykEditor(data: CmykData, parentId: Id) extends Component[Cmyk] {

  def state = {
    Observable.combineLatestMap4(
      data.cyan.changed, data.magenta.changed, data.yellow.changed, data.black.changed
    )(
      (cyan, magenta, yellow, black) => Cmyk(cyan, magenta, yellow, black)
    )
  }

  val min = 0
  val max = 100

  val cyanEditor = new NumberEditor(data.cyan, parentId.child("cyan"), min, max)
  val magentaEditor = new NumberEditor(data.magenta, parentId.child("magenta"), min, max)
  val yellowEditor = new NumberEditor(data.yellow, parentId.child("yellow"), min, max)
  val blackEditor = new NumberEditor(data.black, parentId.child("black"), min, max)

  def view(state: Cmyk) = {
    val color =
      for (
        cyanValue <- state.cyan.right;
        magentaValue <- state.magenta.right;
        yellowValue <- state.yellow.right;
        blackValue <- state.black.right
      ) yield {
        val redValue = 255 * (1-cyanValue/100) * (1-blackValue/100)
        val greenValue = 255 * (1-magentaValue/100) * (1-blackValue/100)
        val blueValue = 255 * (1-yellowValue/100) * (1-blackValue/100)
        ExampleUtils.viewColor(redValue, greenValue, blueValue)
      }
    Observable.combineLatestMap4(
      cyanEditor.viewChanged, magentaEditor.viewChanged, yellowEditor.viewChanged, blackEditor.viewChanged
    )(
      (cyanView, magentaView, yellowView, blackView) =>
        <.div(
          <.table(<.tbody(
            <.tr(<.td("Cyan: "), <.td(cyanView)),
            <.tr(<.td("Magenta: "), <.td(magentaView)),
            <.tr(<.td("Yellow: "), <.td(yellowView)),
            <.tr(<.td("Black: "), <.td(blackView))
          )),
          color.right.getOrElse[ReactElement](<.span("Error"))
        )
    )
  }

  def react(state: Cmyk) = {
    Observable.merge(
      cyanEditor.reacted,
      magentaEditor.reacted,
      yellowEditor.reacted,
      blackEditor.reacted
    )
  }

}