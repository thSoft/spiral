package hu.thsoft.spiral.editor

import hu.thsoft.spiral.data._
import hu.thsoft.spiral.{Component, Id}
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._

object GenericEditor {

  /**
    * Returns a generic editor for the given Data.
    * Using this editor is not very convenient, but you just define a data model and you get an editor for free.
    */
  def apply(data: Data, id: Id): Component = {
    data match {
      case data: BooleanData => new BooleanEditor(data, id)
      case data: StringData => new StringEditor(data, id)
      case data: NumberData => new NumberEditor(data, id)
      case data: ReferenceData[_] => new ReferenceEditor(data, id)
      case data: ListData[_] => new ListEditor(data, id)
      case data: RecordData => new RecordEditor(data, id)
      case data: ChoiceData[_] => new ChoiceEditor(data, id)
    }
  }

}
