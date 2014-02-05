package com.github.ktonga.tweetvirality.models.twitter

import akka.actor._
import akka.pattern.pipe
import scala.concurrent.{ExecutionContext, Future}
import spray.http._
import spray.client.pipelining._
import spray.http.HttpRequest
import scala.util.Failure
import scala.Some
import scala.util.Success
import play.api.libs.concurrent.Akka
import play.api.Play.current

trait TwitterApi { val twitterApi: ActorRef }

trait TwitterApiMain extends TwitterApi {
  val twitterApi = Akka.system.actorOf(Props[TwitterRestApi], "tw-api")
}

object TwitterRestApi {
  val uri = Uri("https://api.twitter.com/1.1")

  type UserIdentifier = Option[Either[Long, String]]

  // Protocol
  case class GetReTweets(id: Long)
  case class ReTweets(rts: List[ReTweet])
  case class GetFollowers(idOrScreenName: UserIdentifier)
  case class Followers(ids: List[Long])
  case class GetFriends(idOrScreenName: UserIdentifier)
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
  import Util.withLogging

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
      withLogging(response, (ids: Ids) => s"Followers: ${ids.ids.size}").map(ids => Followers(ids.ids)) pipeTo sender
    case GetFriends(idOrScreenName) =>
      val response: Future[Ids] = flwPipeline(Get(s"$uri/friends/ids.json?count=5000${userParam(idOrScreenName)}"))
      withLogging(response, (ids: Ids) => s"Friends: ${ids.ids.size}").map(ids => Friends(ids.ids)) pipeTo sender
    case UsersLookupPage(ids) =>
      println(s"Users Lookup Page: ${ids.size}")
      val response: Future[List[User]] = usrPipeline(Post(s"$uri/users/lookup.json",
          FormData(Map("include_entities" -> "false", "user_id" -> ids.mkString(","))))
      )
      withLogging(response, (r: List[User]) => s"Users Lookup Page: ${r.head.toString}")
      response.map(usrs => Users(usrs)) pipeTo sender
    case UsersLookup(ids) =>
      val client = sender
      println(s"Users Lookup: ${ids.size}")
      val worker = context.actorOf(Props(new UsersLookupWorker(client)), "user-lookup-worker")
      worker ! UsersLookupWorker.Page(ids.splitAt(100))
  }

  def userParam(idOrScreenName: Option[Either[Long, String]]) = idOrScreenName match {
    case None => ""
    case Some(Left(id)) => s"&user_id=$id"
    case Some(Right(screenName)) => s"&screen_name=$screenName"
  }
}

object UsersLookupWorker {
  case class Page(idsAndRemaining: (List[Long], List[Long]))
}

class UsersLookupWorker(val clientRef: ActorRef) extends Actor {

  import TwitterRestApi._
  import UsersLookupWorker._

  val twitter = context.parent
  var users = List[User]()
  var remainingIds = List[Long]()

  context.watch(clientRef)
  println(s"Watching client ${clientRef.path.name}")

  def receive: Receive = {
    case Page((ids, remaining)) =>
      remainingIds = remaining
      twitter ! UsersLookupPage(ids)
      println(s"Page - ids: ${ids.size} - rem: ${remaining.size}")
    case Users(_users) =>
      users ++= _users
      if(remainingIds.isEmpty) {
        clientRef ! Users(users)
        self ! PoisonPill
        println(s"Users Done: ${users.size}")
      } else {
        self ! Page(remainingIds.splitAt(100))
      }
    case Terminated(ref) => println(s"Client ${ref.path.name} terminated!")
  }

  override def postStop(): Unit = {
    super.postStop()
    println(s"Worker ${self.path.name} Dead")
  }
}

object Util {
  def withLogging[A](ftr: Future[A], f: A => String)(implicit executor: ExecutionContext): Future[A] = {
    ftr onComplete {
      case Success(r) => println(f(r))
      case Failure(t) => t.printStackTrace()
    }
    ftr
  }
}

