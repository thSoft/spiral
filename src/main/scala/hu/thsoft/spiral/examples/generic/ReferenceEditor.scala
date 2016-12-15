package hu.thsoft.spiral.examples.generic

import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.{Data, ListData, ReferenceData}
import monix.reactive.Observable

case class ReferenceState[Referred <: Data](selected: Stored[Referred], available: List[Referred])

case class Remote[D <: Data, T](data: D, value: T)

class ReferenceEditor[Referred <: Data](data: ReferenceData[Referred], id: Id) extends Component {

  type State = ReferenceState[Referred]

  def state = {
    Observable.combineLatestMap2(
      data.referredChanged, data.scope
    )(
      (selected, available) => ReferenceState(selected, available)
    )
  }

  def output(state: State) = {
    def makeRemoteName(referred: Referred): Observable[Remote[Referred, Stored[String]]] = {
      data.getDisplayedName(referred).map(Remote(referred, _))
    }
    val choiceList: Observable[ChoiceList[Option[Referred]]] =
      ObservableUtils.combineLatestList(state.available.map(makeRemoteName)).map(remoteNames => {
        val none = Choice(Option.empty[Referred], "(none)")
        val choices = remoteNames.map(remoteName => {
          Choice(Option(remoteName.data), remoteName.value.right.toOption.getOrElse("(can't determine name)"))
        })
        val allChoices = none +: choices
        val selectedItem = state.selected.right.toOption
        new ChoiceList(id, allChoices, selectedItem)()
      })
    val view = choiceList.map(_.view)
    val reaction =
      choiceList.switchMap(_.changed).map(selectedReferred => {
        selectedReferred.fold(data.delete)(data.setReferred(_))
      })
    Output(view, reaction)
  }

}