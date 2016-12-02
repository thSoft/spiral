package hu.thsoft.spiral.examples.misc

import fr.hmil.roshttp.HttpRequest
import hu.thsoft.spiral._
import hu.thsoft.spiral.data.Data.Stored
import hu.thsoft.spiral.data.StringData
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import monix.reactive.Observable
import upickle.Js
import upickle.json._

class CatGifs(data: StringData, id: Id) extends Component {

  type State = Stored[String]

  def state = data.changed

  def output(state: State) = {
    val fetch = new Clickable(<.button(_))(id.child("fetch"))("i can haz moar pls?")
    val view: Observable[ReactElement] =
      Observable.pure(<.div(
        fetch.view,
        state.right.toOption.map(url => <.div(<.img(^.src := url)))
      ))
    val requests = fetch.clicked.map(_ => HttpRequest("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats"))
    val responses = HTTP.responses(requests)
    val reaction = responses.map(response => {
      val value = read(response.body)("data")("image_url")
      value match {
        case Js.Str(url) => data.set(url)
        case _ => Action.nop
      }
    })
    Output(view, reaction)
  }

}