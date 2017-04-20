package au.com.simplemachines.sjq

import org.specs2._

object PlayJsonSpec extends Specification {
  def is = s2"""
    Can run jq queries on play json documents   $playJsonExample
    """

  def playJsonExample = {
    import play.api.libs.json._
    import Isos.play

    val json: JsValue = Json.obj(
      "name" -> "Watership Down",
      "location" -> Json.obj("lat" -> 51.235685, "long" -> -1.309197),
      "residents" -> Json.arr(
        Json.obj(
          "name" -> "Fiver",
          "age" -> 4,
          "role" -> JsNull
        ),
        Json.obj(
          "name" -> "Bigwig",
          "age" -> 6,
          "role" -> "Owsla"
        )
      )
    )

    SJQ.query(".residents", json) === Json.arr(
      Json.obj(
        "name" -> "Fiver",
        "age" -> 4,
        "role" -> JsNull
      ),
      Json.obj(
        "name" -> "Bigwig",
        "age" -> 6,
        "role" -> "Owsla"
      )
    )
  }
}
