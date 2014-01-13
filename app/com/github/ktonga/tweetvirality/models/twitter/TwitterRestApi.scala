package com.github.ktonga.tweetvirality.models.twitter

import akka.actor.Actor
import akka.pattern.pipe
import scala.concurrent.Future
import spray.http.{HttpRequest, Uri}
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import spray.client.pipelining._

object TwitterRestApi {
  val uri = Uri("https://api.twitter.com/1.1")

  // Protocol
  case class GetReTweets(id: Long)
  case class ReTweets(rts: List[ReTweet])
  case class GetFollowers(id: Long)
  case class Followers(followers: List[Long])
}

class TwitterRestApi extends Actor
  with OAuthTwitterAuthorization
  with DefaultJsonProtocol
  with SprayJsonSupport {

  import TwitterRestApi._
  import context.dispatcher

  implicit val userFormat = jsonFormat4(User)
  implicit val tweetFormat = jsonFormat2(Tweet)
  implicit val retweetFormat = jsonFormat3(ReTweet)
  implicit val idsFormat = jsonFormat1(Ids)

  val rtPipeline: HttpRequest => Future[List[ReTweet]] = (
    authorize
    ~> sendReceive(context, context.dispatcher)
    ~> unmarshal[List[ReTweet]]
  )

  val flwPipeline: HttpRequest => Future[Ids] = (
    authorize
    ~> sendReceive(context, context.dispatcher)
    ~> unmarshal[Ids]
  )

  def receive: Receive = {
    case GetReTweets(id) =>
      val response: Future[List[ReTweet]] = rtPipeline(Get(s"$uri/statuses/retweets/$id.json?count=100"))
      response.map(l => ReTweets(l)) pipeTo sender
    case GetFollowers(id) =>
      val response: Future[Ids] = flwPipeline(Get(s"$uri/followers/ids.json?user_id=$id&count=5000"))
      response recover {
        case t =>
          println(t.getMessage)
          Ids(List())
      } map(ids => Followers(ids.ids)) pipeTo sender
  }
}

