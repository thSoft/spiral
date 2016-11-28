package hu.thsoft.spiral

import org.scalajs.dom.raw.URL
import monix.reactive.Observable
import fr.hmil.roshttp.HttpRequest
import fr.hmil.roshttp.response.HttpResponse
import fr.hmil.roshttp.response.SimpleHttpResponse
import monix.execution.Scheduler.Implicits.global

object HTTP {

  def responses(requests: Observable[HttpRequest]): Observable[SimpleHttpResponse] = {
    requests.switchMap(request => {
      Observable.fromFuture(request.send())
    })
  }

}