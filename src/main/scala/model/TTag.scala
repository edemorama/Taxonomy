package model

/**
  * Created by edem on 28/07/16.
  */

/**
  * Taxonomy tag with translation locales
  *
  * @param name - Tag name
  * @param locales - Sequence of locale values for Tag name
  */
case class TTag(name : String, locales: Seq[(String, String)]) {
  /**
    * TODO Equality implementation
    * @param that
    * @return
    */
  def equals(that: TTag) = ???
}

