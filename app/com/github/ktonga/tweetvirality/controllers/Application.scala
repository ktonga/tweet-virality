package com.github.ktonga.tweetvirality.controllers

import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(com.github.ktonga.tweetvirality.views.html.index())
  }

}
