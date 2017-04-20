package au.com.simplemachines.sjq

import scalaz.Isomorphism._
import play.api.libs.json._
import argonaut.{Argonaut, Json}
import Argonaut.{jNumber, jString, jArray}

object Isos {
  implicit def play: IsoSet[JsValue, Json] = new IsoSet[JsValue, Json] { self =>
    val to: JsValue => Json = jsValue => jsValue match {
      case JsString(s)      => jString(s)
      case JsNumber(n)      => jNumber(n)
      case JsObject(fields) => Json(fields.mapValues(self.to).toSeq: _*)
      case JsArray(values)  => jArray(values.toList.map(self.to))
      case JsNull           => Json.jNull
    }

    val from: Json => JsValue = json => if (json.isString) {
      JsString(json.stringOr(???))
    } else if (json.isNumber) {
      JsNumber(json.numberOr(???).toBigDecimal)
    } else if (json.isObject) {
      val obj = json.objectOr(???)
      JsObject(obj.toMap.mapValues(self.from).toList)
    } else if (json.isArray) {
      JsArray(json.arrayOr(???).map(self.from))
    } else if (json.isNull)
      JsNull
    else ???
  }
}
