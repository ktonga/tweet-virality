package com.github.ktonga.tweetvirality.models.twitter

import akka.actor.{Props, PoisonPill, ActorRef, Actor}
import akka.pattern.pipe
import scala.concurrent.Future
import spray.http._
import spray.client.pipelining._
import scala.util.{Failure, Success}
import spray.http.HttpRequest

object TwitterRestApi {
  val uri = Uri("https://api.twitter.com/1.1")

  // Protocol
  case class GetReTweets(id: Long)
  case class ReTweets(rts: List[ReTweet])
  case class GetFollowers(idOrScreenName: Option[Either[Long, String]])
  case class Followers(ids: List[Long])
  case class GetFriends(idOrScreenName: Option[Either[Long, String]])
  case class Friends(ids: List[Long])
  case class UsersLookup(ids: List[Long])
  case class UsersLookupPage(ids: List[Long])
  case class Users(users: List[User])
}

class TwitterRestApi extends Actor
  with OAuthTwitterAuthorization
  with TwitterJsonProtocol {

  import TwitterRestApi._
  import context.dispatcher

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

  val usrPipeline: HttpRequest => Future[List[User]] = (
    authorize
    ~> sendReceive(context, context.dispatcher)
    ~> unmarshal[List[User]]
  )

  def receive: Receive = {
    case GetReTweets(id) =>
      val response: Future[List[ReTweet]] = rtPipeline(Get(s"$uri/statuses/retweets/$id.json?count=100"))
      response.map(l => ReTweets(l)) pipeTo sender
    case GetFollowers(idOrScreenName) =>
      val response: Future[Ids] = flwPipeline(Get(s"$uri/followers/ids.json?count=5000${userParam(idOrScreenName)}"))
      response.map(ids => Followers(ids.ids)) pipeTo sender
    case GetFriends(idOrScreenName) =>
      val response: Future[Ids] = flwPipeline(Get(s"$uri/friends/ids.json?count=5000${userParam(idOrScreenName)}"))
      response.map(ids => Friends(ids.ids)) pipeTo sender
    case UsersLookupPage(ids) =>
      println(s"Users Lookup Page: ${ids.size}")
      val response: Future[List[User]] = usrPipeline(Post(s"$uri/users/lookup.json",
          FormData(Map("include_entities" -> "false", "user_id" -> ids.mkString(","))))
      )
      response onComplete {
        case Success(result) =>
          println(s"Users Lookup Page: ${result.head.toString}")
        case Failure(t) => t.printStackTrace()
      }
      response.map(usrs => Users(usrs)) pipeTo sender
    case UsersLookup(ids) =>
      println(s"Users Lookup: ${ids.size}")
      val worker = context.actorOf(Props(new UsersLookupWorker(sender)))
      worker ! UsersLookupWorker.Page(ids.splitAt(100))
  }

  def userParam(idOrScreenName: Option[Either[Long, String]]) = idOrScreenName match {
    case None => ""
    case Some(Left(id)) => s"&user_id=$id"
    case Some(Right(screenName)) => s"&screen_name=$screenName"
  }
}

object UsersLookupWorker {
  case class Start(allIds: List[Long])
  case class Page(idsAndRemaining: (List[Long], List[Long]))
}

class UsersLookupWorker(val clientRef: ActorRef) extends Actor {

  import TwitterRestApi._
  import UsersLookupWorker._

  val twitter = context.parent
  var users = List[User]()
  var remainingIds = List[Long]()

  def receive: Receive = {
    case Page((ids, remaining)) =>
      remainingIds = remaining
      twitter ! UsersLookupPage(ids)
    case Users(_users) =>
      users ++= _users
      if(remainingIds.isEmpty) {
        clientRef ! Users(users)
        self ! PoisonPill
      } else {
        self ! Page(remainingIds.splitAt(100))
      }
  }

}

