package hu.thsoft.spiral.examples.color

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.data._

object Color {

  val data = new ColorData(new FirebaseDataStore(new Firebase("https://thsoft.firebaseio.com/spiral/examples/color")))

}

abstract class ColorCase(dataStore: DataStore) extends RecordData(dataStore, "Color")

class RgbData(dataStore: DataStore) extends ColorCase(dataStore) {

  val red = field("Red", new NumberData(_)(0, 255))

  val green = field("Green", new NumberData(_)(0, 255))

  val blue = field("Blue", new NumberData(_)(0, 255))

  def fields = Seq(red, green, blue)

}

class CmykData(dataStore: DataStore) extends ColorCase(dataStore) {

  val cyan = field("Cyan", new NumberData(_)(0, 100))

  val magenta = field("Magenta", new NumberData(_)(0, 100))

  val yellow = field("Yellow", new NumberData(_)(0, 100))

  val black = field("Black", new NumberData(_)(0, 100))

  def fields = Seq(cyan, magenta, yellow, black)

}

class ColorData(dataStore: DataStore) extends ChoiceData[ColorCase](dataStore) {

  def cases = Seq(
    Case[ColorCase]("RGB", new RgbData(_)),
    Case[ColorCase]("CMYK", new CmykData(_))
  )

}