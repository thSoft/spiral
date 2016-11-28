package hu.thsoft.spiral.examples

import scala.scalajs.js.JSApp

import org.scalajs.dom._

import fr.hmil.roshttp.HttpRequest
import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.Action
import hu.thsoft.spiral.Clickable
import hu.thsoft.spiral.Component
import hu.thsoft.spiral.Data.Stored
import hu.thsoft.spiral.HTTP
import hu.thsoft.spiral.Id
import hu.thsoft.spiral.Output
import hu.thsoft.spiral.StringData
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

object CatGifsApp extends JSApp {

  def main() {
    ExampleUtils.runComponent(new CatGifs(new StringData(new Firebase("https://thsoft.firebaseio.com/spiral/examples/catGifUrl")), Id.root))
  }

}