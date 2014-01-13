package models

import models.TwitterRestApi._
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import play.api.libs.concurrent.Akka
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future
import play.api.Play.current

object RtTreeBuilder {

  val UnknownUser = User(-1, "Unknown", "unknown", "/assets/images/unknown-person.png")

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

  def getFollowers(id: Long) = (twitterApi ? GetFollowers(id)).mapTo[Followers].map(_.followers.toSet)

}

class Node(val n: Int, var ns: List[Node]) {
  def this(n: Int) = this(n, List())
}

object Test extends App {
  // 10
  //  45
  //   845
  //   217
  //  76
  //   354
  //   89
  //  534
  //   234
  //   150
  //  -1
  //   703
  //   734
  val root = 10
  val numbers = List(703, 734, 150, 234, 89, 354, 217, 845, 534, 76, 45)
  def children(n: Int): Set[Int] = n match {
    case 10 => Set(45, 76, 534)
    case 45 => Set(845, 217)
    case 76 => Set(354, 89)
    case 534 => Set(234, 150)
    case _ => Set()
  }

  def printNode(node: Node, indent: String): Unit = {
    println(indent + node.n)
    node.ns.foreach(e => printNode(e, indent + "  "))
  }

  def level(parentNodes: List[Node], others: List[Int]): (List[Node], List[Int]) = {
    parentNodes.foldLeft(List[Node](), others) {
      (partial: (List[Node], List[Int]), node: Node) =>
        val (contains, notcont) = cont(node.n, partial._2)
        val ns: List[Node] = contains.map(n => new Node(n))
        node.ns = ns
        (partial._1 ++ ns, notcont)
    }
  }

  def cont(number: Int, others: List[Int]): (List[Int], List[Int]) = {
    val c = children(number)
    others.partition(c.contains)
  }

  val rootNode = new Node(root)

  def rec(lastLevel: List[Node], remaining: List[Int]): List[Int] = {
    if(lastLevel.nonEmpty)  {
      val (withParent, missing) = level(lastLevel, remaining)
      rec(withParent, missing)
    } else remaining
  }

  var remaining = rec(List(rootNode), numbers)
  rootNode.ns = rootNode.ns :+ new Node(-1, remaining.map(r => new Node(r)))

  printNode(rootNode, "")
}