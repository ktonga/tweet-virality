package com.github.ktonga.tweetvirality.models.people

import com.github.ktonga.tweetvirality.models.twitter._
import TwitterRestApi._
import TwitterCache._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Future.successful
import akka.pattern.ask

trait Relationship {
  def followers(user: Option[String]): Future[Set[Long]]
  def friends(user: Option[String]): Future[Set[Long]]
}
trait UsersLookup {
  def usersLookup(ids: List[Long]): Future[List[User]]
}
trait Friendships {
  def friendships(yourFriends: Set[Long], yourFollowers: Set[Long],
        hisFriends: Set[Long], hisFollowers: Set[Long]): Set[(Long, Boolean, Boolean, Boolean, Boolean)]
}

trait DefaultAskTimeout {
  implicit val timeout: Timeout = 10.seconds
}

trait PeopleInCommonMain {
  this: Relationship
    with Friendships
    with UsersLookup =>

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def peopleInCommon(screenName: String): Future[List[Person]] = {
    val yourFriendsFtr = friends(None)
    val yourFollowersFtr = followers(None)
    val hisFriendsFtr = friends(Some(screenName))
    val hisFollowersFtr = followers(Some(screenName))

    val friendshipsFtr = for {
      yourFriends <- yourFriendsFtr
      yourFollowers <- yourFollowersFtr
      hisFriends <- hisFriendsFtr
      hisFollowers <- hisFollowersFtr
    } yield friendships(yourFriends, yourFollowers, hisFriends, hisFollowers)

    for {
      friendships <- friendshipsFtr.map(_.toList)
      users <- usersLookup(friendships.map(_._1))
    } yield toPeople(friendships, users)

  }

  private def toPeople(friendships: List[(Long, Boolean, Boolean, Boolean, Boolean)], users: List[User]) = {
    val usersById = users.map(u => (u.id, u)).toMap
    friendships collect {
      case f if usersById.contains(f._1) =>
        val user = usersById(f._1)
        Person(user, f._2, f._3, f._4, f._5)
    }
  }

}

trait RelationshipMain extends Relationship with DefaultAskTimeout {
  this: TwitterApi with TwitterCaching =>

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def relationship(apiMsg: UserIdentifier => Any, cacheMsg: String => Any, saveMsg: (String, Set[Long]) => Any): Option[String] => Future[Set[Long]] = {
    nameOpt =>
      val name = nameOpt.getOrElse("ktonga")

      val cacheFtr = (twitterCache ? cacheMsg(name)) map {
        case FollowersIds(ids) => ids
        case FriendsIds(ids) => ids
      }

      cacheFtr recoverWith {
        case t: MissingFromCacheException =>
          val apiFtr = (twitterApi ? apiMsg(nameOpt.map(n => Right(n)))) map {
            case Followers(ids) => ids.toSet
            case Friends(ids) => ids.toSet
          }
          apiFtr onSuccess {
            case ids => twitterCache ! saveMsg(name, ids)
          }
          apiFtr
      }
  }

  val followersFunc = relationship(GetFollowers.apply, FollowersFor.apply, SaveFollowers.apply)
  val friendsFunc = relationship(GetFriends.apply, FriendsFor.apply, SaveFriends.apply)

  def followers(user: Option[String]): Future[Set[Long]] = followersFunc(user)
  def friends(user: Option[String]): Future[Set[Long]] = friendsFunc(user)
}

trait UsersLookupMain extends UsersLookup with DefaultAskTimeout {
  this: TwitterApi with TwitterCaching =>

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def usersLookup(ids: List[Long]): Future[List[User]] = {
    val cacheFtr = (twitterCache ? UsersFor(ids)) map {
      case CachedUsers(users) => users
    }
    cacheFtr flatMap { cachedUsers =>
      val missing = ids.toSet &~ cachedUsers.map(_.id).toSet
      println(s"Cached users - hit: ${cachedUsers.size}, missing: ${missing.size}")
      if(missing.isEmpty) successful(cachedUsers)
      else {
        val fetchedUsers = (twitterApi ? UsersLookup(missing.toList)) map {case Users(users) => users}
        fetchedUsers onSuccess {
          case users => twitterCache ! SaveUsers(users)
        }
        fetchedUsers map (_ ++ cachedUsers)
      }
    }
  }
}

trait FriendshipsMain extends Friendships {

  def friendships(yourFriends: Set[Long], yourFollowers: Set[Long],
                       hisFriends: Set[Long], hisFollowers: Set[Long]) = {

    val hisFnF = hisFriends ++ hisFollowers

    val yourFriendships =
      (yourFriends &~ yourFollowers).map((_, true, false)) ++
        (yourFriends & yourFollowers).map((_, true, true)) ++
        (yourFollowers &~ yourFriends).map((_, false, true))

    yourFriendships collect {
      case (id, friend, follower) if hisFnF.contains(id) =>
        (id, friend, follower, hisFriends.contains(id), hisFollowers.contains(id))
    }
  }
}

trait PeopleInCommon
  extends PeopleInCommonMain
  with FriendshipsMain
  with RelationshipMain
  with UsersLookupMain
  with TwitterCachingMain
  with TwitterApiMain

