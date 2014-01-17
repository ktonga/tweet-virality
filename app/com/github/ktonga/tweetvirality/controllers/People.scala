package com.github.ktonga.tweetvirality.controllers

import play.api.mvc._
import play.api.libs.ws._
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor.Props
import akka.pattern.ask
import play.api.libs.concurrent.Akka
import play.api.Play.current
import akka.util.Timeout
import com.github.ktonga.tweetvirality.models.twitter.{User, TwitterRestApi}
import com.github.ktonga.tweetvirality.models.twitter.TwitterRestApi._
import com.github.ktonga.tweetvirality.models.people.PeopleInCommonBuilder

object People extends Controller {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  val twitterApi = Akka.system.actorOf(Props[TwitterRestApi])

  implicit val timeout: Timeout = 30.seconds

  def people(screenName: String) = Action.async {
    PeopleInCommonBuilder(screenName) map { people =>
      Ok(com.github.ktonga.tweetvirality.views.html.people(User(-1, "", screenName, "", ""), people))
    }
  }

  def embeddedTweet(id: Long): Future[String] = {
    val tweetFtr = WS.url(s"https://api.twitter.com/1/statuses/oembed.json?id=$id").get()
    tweetFtr.map(r => (r.json \ "html").as[String])
  }

  def hardcodedTweet = future {
    """<blockquote class="twitter-tweet"><p>Vim config by <a href="https://twitter.com/jboner">@jboner</a> for coding like a man. <a
      href="http://t.co/mRkz5Ccrpa">http://t.co/mRkz5Ccrpa</a></p>&mdash; Tonga (@ktonga) <a
      href="https://twitter.com/ktonga/statuses/417741964227117056">December 30, 2013</a></blockquote>
      <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>"""
  }
}
