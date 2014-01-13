package com.github.ktonga.tweetvirality.models

package object twitter {

  // API Model
  case class User(id: Long, name: String, screen_name: String, profile_image_url: String)
  case class Tweet(id: Long, user: User)
  case class ReTweet(id: Long, user: User, retweeted_status: Tweet)
  case class Ids(ids: List[Long])

}
