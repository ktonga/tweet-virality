package com.github.ktonga.tweetvirality.models.people

import play.api.libs.concurrent.Akka
import akka.actor.Props
import com.github.ktonga.tweetvirality.models.twitter.{User, Friendship, TwitterRestApi}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import TwitterRestApi._
import akka.pattern.ask
import play.api.Play.current

object PeopleInCommonBuilder {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  val twitterApi = Akka.system.actorOf(Props[TwitterRestApi])

  implicit val timeout: Timeout = 10.seconds

  def apply(screenName: String): Future[List[Person]] = {
    val yourFriendsFtr = (twitterApi ? GetFriends(None)) map {case Friends(ids) => ids.toSet}
    val yourFollowersFtr = (twitterApi ? GetFollowers(None)) map {case Followers(ids) => ids.toSet}
    val hisFriendsFtr = (twitterApi ? GetFriends(Some(Right(screenName)))) map {case Friends(ids) => ids.toSet}
    val hisFollowersFtr = (twitterApi ? GetFollowers(Some(Right(screenName)))) map {case Followers(ids) => ids.toSet}

    val friendshipsFtr = for {
      yourFriends <- yourFriendsFtr
      yourFollowers <- yourFollowersFtr
      hisFriends <- hisFriendsFtr
      hisFollowers <- hisFollowersFtr
    } yield buildFriendships(yourFriends, yourFollowers, hisFriends, hisFollowers)

    for {
      friendships <- friendshipsFtr.map(_.toList)
      users <- (twitterApi ? UsersLookup(friendships.map(_._1)))  map {case Users(users) => users}
    } yield toPeople(friendships, users)

  }

  def buildFriendships(yourFriends: Set[Long], yourFollowers: Set[Long],
                  hisFriends: Set[Long], hisFollowers: Set[Long]) = {

    val hisFnF = hisFriends ++ hisFollowers

    val yourFriendships =
      (yourFriends &~ yourFollowers).map((_, true, false)) ++
      (yourFriends & yourFollowers).map((_, true, true)) ++
      (yourFollowers &~ yourFriends).map((_, true, true))

    yourFriendships collect {
      case (id, friend, follower) if hisFnF.contains(id) =>
        (id, friend, follower, hisFriends.contains(id), hisFollowers.contains(id))
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
