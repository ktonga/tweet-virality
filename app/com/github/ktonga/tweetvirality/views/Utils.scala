package com.github.ktonga.tweetvirality.views

object Utils {

  def flagIcon(flag: Boolean) = if(flag) "ok" else "remove"
  def compoundIcon(follow: Boolean, followedBy: Boolean) =
    if(follow && followedBy) "transfer"
    else if(follow) "arrow-right"
    else "arrow-left"

}
