package com.github.ktonga.tweetvirality.models.people

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class PeopleInCommonSpec extends Specification with ScalaCheck {

    "Friendships find relationship between users" ! foo

    import org.scalacheck._

    val friendships = new FriendshipsMain {}

    val idGen = Gen.choose(1L, 100000L)
    val idsGen = Gen.containerOfN[Set, Long](200, idGen)

    def foo = Prop.forAll(idsGen, idsGen, idsGen, idsGen) {
      (yfrs: Set[Long], yfos: Set[Long], hfrs: Set[Long], hfos: Set[Long]) =>
        val fshp = friendships.friendships(yfrs, yfos, hfrs, hfos)
        fshp forall {
          case (id, yfr, yfo, hfr, hfo) =>
            val (cyfr, cyfo, chfr, chfo) = (yfrs.contains(id), yfos.contains(id), hfrs.contains(id), hfos.contains(id))
            val valid = ( (yfr || yfo) && (hfr || hfo) ) && yfr == cyfr && yfo == cyfo && hfr == chfr && hfo == chfo
            if(!valid) println(s"$id: ($yfr, $yfo, $hfr, $hfo) -> ($cyfr, $cyfo, $chfr, $chfo)")
            valid
        } must beTrue
    }

}

//class Specs2Spec extends