package hu.thsoft.spiral

import fr.hmil.roshttp.HttpRequest
import fr.hmil.roshttp.response.SimpleHttpResponse
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

object HTTP {

  def responses(requests: Observable[HttpRequest]): Observable[SimpleHttpResponse] = {
    requests.switchMap(request => {
      Observable.fromFuture(request.send())
    })
  }

}