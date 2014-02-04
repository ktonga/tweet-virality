package com.github.ktonga.tweetvirality.models.people

import play.api.libs.concurrent.Akka
import akka.actor.Props
import com.github.ktonga.tweetvirality.models.twitter.{TwitterCache, User, TwitterRestApi, MissingFromCacheException}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Future.successful
import TwitterRestApi._
import akka.pattern.ask
import play.api.Play.current
import TwitterCache._
import org.scalacheck.Properties

object PeopleInCommonBuilder extends FriendshipsBuilder {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  val twitterCache = Akka.system.actorOf(Props(new TwitterCache("/tmp/twitter/")), "tw-cache")
  val twitterApi = Akka.system.actorOf(Props[TwitterRestApi], "tw-api")

  implicit val timeout: Timeout = 10.seconds

  def apply(screenName: String): Future[List[Person]] = {
    val yourFriendsFtr = friends(None)
    val yourFollowersFtr = followers(None)
    val hisFriendsFtr = friends(Some(screenName))
    val hisFollowersFtr = followers(Some(screenName))

    val friendshipsFtr = for {
      yourFriends <- yourFriendsFtr
      yourFollowers <- yourFollowersFtr
      hisFriends <- hisFriendsFtr
      hisFollowers <- hisFollowersFtr
    } yield buildFriendships(yourFriends, yourFollowers, hisFriends, hisFollowers)

    for {
      friendships <- friendshipsFtr.map(_.toList)
      users <- usersLookup(friendships.map(_._1))
    } yield toPeople(friendships, users)

  }

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

  val followers = relationship(GetFollowers.apply, FollowersFor.apply, SaveFollowers.apply)
  val friends = relationship(GetFriends.apply, FriendsFor.apply, SaveFriends.apply)

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

  def toPeople(friendships: List[(Long, Boolean, Boolean, Boolean, Boolean)], users: List[User]) = {
    val usersById = users.map(u => (u.id, u)).toMap
    friendships collect {
      case f if usersById.contains(f._1) =>
        val user = usersById(f._1)
        Person(user, f._2, f._3, f._4, f._5)
    }
  }

}

trait FriendshipsBuilder {

  def buildFriendships(yourFriends: Set[Long], yourFollowers: Set[Long],
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

object FriendshipsChecks extends App with FriendshipsBuilder {
  import org.scalacheck._
  val idGen = Gen.choose(1L, 100000L)
  val setGen = Gen.containerOfN[Set, Long](200, idGen)
  Prop.forAll(setGen, setGen, setGen, setGen) {
    (yfrs: Set[Long], yfos: Set[Long], hfrs: Set[Long], hfos: Set[Long]) =>
    val fshp = buildFriendships(yfrs, yfos, hfrs, hfos)
    fshp forall {
      case (id, yfr, yfo, hfr, hfo) =>
        val (cyfr, cyfo, chfr, chfo) = (yfrs.contains(id), yfos.contains(id), hfrs.contains(id), hfos.contains(id))
        val valid = ( (yfr || yfo) && (hfr || hfo) ) && yfr == cyfr && yfo == cyfo && hfr == chfr && hfo == chfo
        if(!valid) println(s"$id: ($yfr, $yfo, $hfr, $hfo) -> ($cyfr, $cyfo, $chfr, $chfo)")
        valid
    }
  }.check
}

