package com.github.ktonga.tweetvirality.models.virality

import scala.concurrent.duration._
import play.api.libs.concurrent.Akka
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future
import play.api.Play.current
import com.github.ktonga.tweetvirality.models.twitter.{ReTweet, TwitterRestApi, User}
import com.github.ktonga.tweetvirality.models.twitter.TwitterRestApi.{Followers, GetFollowers}

object RtTreeBuilder {

  val UnknownUser = User(-1, "Unknown", "unknown", "/assets/images/unknown-person.png", "")

  val twitterApi = Akka.system.actorOf(Props[TwitterRestApi])

  implicit val timeout: Timeout = 10.seconds

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def apply(rts: List[ReTweet]): Future[RtNode] = {

    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    println(s"Building RTs Tree for ${rts.size} retweets")

    val authorNode = user2rtnode(rts.head.retweeted_status.user)

    def rec(lastLevel: List[RtNode], remaining: List[User]): Future[List[User]] =
      if(lastLevel.nonEmpty)  {
        level(lastLevel, remaining).flatMap(l => rec(l._1, l._2))
      } else Future(remaining)

    val remainingF = rec(List(authorNode), rts.map(_.user))
    remainingF map { remaining =>
      val unknownNode = user2rtnode(UnknownUser)
      unknownNode.children = remaining.map(user2rtnode)
      authorNode.children = authorNode.children :+ unknownNode
      authorNode
    }
  }

  def level(parentNodes: List[RtNode], others: List[User]): Future[(List[RtNode], List[User])] =
    parentNodes.foldLeft(Future(List[RtNode](), others)) {
      (partialF: Future[(List[RtNode], List[User])], node: RtNode) =>
        partialF flatMap { partial =>
          findFollowers(node.id, partial._2) map {
            followersAndMissing =>
              val children: List[RtNode] = followersAndMissing._1.map(user2rtnode)
              node.children = children
              (partial._1 ++ children, followersAndMissing._2)
          }
        }
    }

  def findFollowers(id: Long, users: List[User]): Future[(List[User], List[User])] =
    getFollowers(id).map(followers => users.partition( u => followers.contains(u.id) ))

  def user2rtnode(user: User): RtNode =
    RtNode(user.id, user.name, user.screen_name, user.profile_image_url)

  def getFollowers(id: Long) = (twitterApi ? GetFollowers(Some(Left(id)))).mapTo[Followers].map(_.ids.toSet)

}

