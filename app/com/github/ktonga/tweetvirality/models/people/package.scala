package com.github.ktonga.tweetvirality.models

import com.github.ktonga.tweetvirality.models.twitter.User

package object people {

  case class Person(
    user: User,
    youFollow: Boolean, followsYou: Boolean,
    heFollows: Boolean, followsHim: Boolean)

}
