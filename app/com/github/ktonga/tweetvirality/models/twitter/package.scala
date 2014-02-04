package com.github.ktonga.tweetvirality.models

import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import scala.util.control.NoStackTrace

package object twitter {

  // API Model
  case class User(id: Long, name: String, screen_name: String, profile_image_url: String, description: String)
  case class Tweet(id: Long, user: User)
  case class ReTweet(id: Long, user: User, retweeted_status: Tweet)
  case class Ids(ids: List[Long])
  case class Friendship(id: Long, name: String, screen_name: String, connections: List[String])

  trait TwitterJsonProtocol
    extends DefaultJsonProtocol
    with SprayJsonSupport {

    implicit val userFormat = jsonFormat5(User)
    implicit val tweetFormat = jsonFormat2(Tweet)
    implicit val retweetFormat = jsonFormat3(ReTweet)
    implicit val idsFormat = jsonFormat1(Ids)
    implicit val friendshipFormat = jsonFormat4(Friendship)
  }

  class MissingFromCacheException(message: String) extends Exception(message) with NoStackTrace

}
