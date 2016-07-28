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
    * @param that - TTag instance to compare with this instance
    * @return
    */
  def equals(that: TTag) = ???
}

/**
  * Companion object with simple Tag creation
  */
object TTag {
  /**
    * Create a Tag with the en locale based on the tName parameter
    * @param tName - The en locale tag name
    * @return
    */
  def simpleTag(tName: String) = TTag(tName, List(("en_GB", tName)))
}

