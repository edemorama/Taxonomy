package model

import java.util.UUID

import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._
/**
  * Created by edem on 02/07/16.
  */

/**
  * generic tuple2 formatter
  */
trait tuple2Format {
  implicit def tuple2Reads[A,B](implicit aReads: Reads[A], bReads: Reads[B]) = Reads[Tuple2[A, B]] {
    case JsArray(arr) if arr.size == 2 => for {
      a <- aReads.reads(json = arr.head)
      b <- bReads.reads(arr(1))
    } yield (a, b)
    case _ => JsError(Seq(JsPath() -> Seq(ValidationError("Expected array of two elements"))))
  }
  implicit def tuple2Writes[A,B](implicit aWrites: Writes[A], bWrites: Writes[B]) = new Writes[Tuple2[A,B]] {
    def writes(tuple: Tuple2[A,B]) = JsArray(Seq(aWrites.writes(tuple._1), bWrites.writes(tuple._2)))
  }
}

/**
  * Taxonomy Json formatters:
  *   Taxonomy: use TaxonomyS in preference to Taxonomy because it uses Seq[TaxonomyS] to Stream[Taxonomy]
  *   List(String, String): Use case class Locales to provide formatter for List(String, String)
  */
object JsonFormatter extends tuple2Format {

  implicit val ttagFormat : Format[TTag] = (
    (__ \ "name").format[String] and
      (__ \ "locales").lazyFormat(Reads.seq[(String, String)], Writes.seq[(String,String)])
    )(TTag.apply, unlift(TTag.unapply))

 implicit val taxonomyFormat3 : Format[Taxonomy] = (
    (__ \ "data").format[TTag] and
      (__ \ "nodes").lazyFormat(Reads.traversableReads[Stream,Taxonomy], Writes.traversableWrites[Taxonomy]) and
      (__ \ "id").format[UUID]
  )(Taxonomy.apply, unlift(Taxonomy.unapply))

  //alternative Format[Taxonomy] definition
  object taxonomyFormat2 extends Format[Taxonomy] {
    def writes(t: Taxonomy) = Json.obj(
      "data" -> Json.toJson(t.data),
      "nodes" -> Json.toJson(t.nodes),
      "id" -> Json.toJson(t.id)
    )
    def reads(js: JsValue) : JsResult[Taxonomy] = for {
      ttag <- (js \ "data").validate[TTag]
      nodes <- (js \ "nodes").validate[Stream[Taxonomy]]
      id <- (js \ "id").validate[UUID]
    } yield Taxonomy(ttag, nodes, id)
  }
}



