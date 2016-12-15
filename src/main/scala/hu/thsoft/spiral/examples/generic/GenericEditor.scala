package hu.thsoft.spiral.examples.generic

import hu.thsoft.spiral.{Component, Id}
import hu.thsoft.spiral.data._

object GenericEditor {

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
