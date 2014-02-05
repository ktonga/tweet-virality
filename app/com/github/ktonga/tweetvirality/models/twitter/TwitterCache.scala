package com.github.ktonga.tweetvirality.models.twitter

import akka.actor.{Props, ActorRef, Actor}
import akka.actor.Status.Failure
import play.api.libs.concurrent.Akka
import play.api.Play.current

trait TwitterCaching { val twitterCache: ActorRef }

trait TwitterCachingMain extends TwitterCaching {
  val twitterCache = Akka.system.actorOf(Props(new TwitterCache("/tmp/twitter/")), "tw-cache")
}

object TwitterCache {
  case class FollowersFor(name: String)
  case class FriendsFor(name: String)
  case class FollowersIds(ids: Set[Long])
  case class FriendsIds(ids: Set[Long])
  case class SaveFollowers(name: String, ids: Set[Long])
  case class SaveFriends(name: String, ids: Set[Long])
  case class UsersFor(ids: List[Long])
  case class CachedUsers(users: List[User])
  case class SaveUsers(users: List[User])


  val missing = Failure(new MissingFromCacheException("Missing Resource"))
}

class TwitterCache(path: String) extends Actor {
  import TwitterCache._

  var followersCache = Map[String, Set[Long]]()
  var friendsCache = Map[String, Set[Long]]()
  var usersCache = Map[Long, User]()

  override def receive: Actor.Receive = {
    case FollowersFor(name) =>
      sender ! followersCache.get(name).map(FollowersIds.apply).getOrElse(missing)
    case FriendsFor(name) =>
      sender ! friendsCache.get(name).map(FriendsIds.apply).getOrElse(missing)
    case SaveFollowers(name, ids) => followersCache += name -> ids
    case SaveFriends(name, ids) => friendsCache += name -> ids
    case UsersFor(ids) =>
      val users = ids.map(id => usersCache.get(id))
      sender ! CachedUsers(users.flatten)
    case SaveUsers(users) =>
      usersCache ++= users.map(u => (u.id, u))
  }

}
