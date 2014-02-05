package com.github.ktonga.tweetvirality.controllers

import play.api.mvc._
import com.github.ktonga.tweetvirality.models.twitter.User
import com.github.ktonga.tweetvirality.models.people.PeopleInCommon
import com.github.ktonga.tweetvirality.views

object People extends Controller with PeopleInCommon {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def people(screenName: String) = Action.async {
    peopleInCommon(screenName) map { people =>
      Ok(views.html.people(User(-1, "", screenName, "", ""), people))
    }
  }

}
