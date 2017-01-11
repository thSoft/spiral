package hu.thsoft.spiral.examples

import firebase.Firebase
import hu.thsoft.spiral.data._
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._

object ColorEditor {

  val data = new ColorData(Database.dataStore("spiral/examples/color"))

  def viewColor(red: Double, green: Double, blue: Double): ReactElement = {
    <.div(^.backgroundColor := s"rgb(${red.toInt}, ${green.toInt}, ${blue.toInt})", "\u00a0")
  }

}

abstract class ColorCase(dataStore: DataStore) extends RecordData(dataStore, "Color")

class RgbData(dataStore: DataStore) extends ColorCase(dataStore) {

  private val max = 255

  val red = field("Red", new NumberData(_)(0, max))

  val green = field("Green", new NumberData(_)(0, max))

  val blue = field("Blue", new NumberData(_)(0, max))

  def fields = Seq(red, green, blue)

}

class CmykData(dataStore: DataStore) extends ColorCase(dataStore) {

  private val max = 100

  val cyan = field("Cyan", new NumberData(_)(0, max))

  val magenta = field("Magenta", new NumberData(_)(0, max))

  val yellow = field("Yellow", new NumberData(_)(0, max))

  val black = field("Black", new NumberData(_)(0, max))

  def fields = Seq(cyan, magenta, yellow, black)

}

class ColorData(dataStore: DataStore) extends ChoiceData[ColorCase](dataStore) {

  def cases = Seq(
    Case("RGB", new RgbData(_)),
    Case("CMYK", new CmykData(_))
  )

}