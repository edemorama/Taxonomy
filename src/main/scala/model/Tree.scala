package model

import java.util.UUID

import play.api.libs.json.JsValue

/**
  * Created by edem on 30/06/16.
  */

/**
  * Generic Tree with data and an id
  * @tparam A - data parameter type
  * @tparam B - id parameter type
  */
trait Tree[A,B] {
  def data : A
  def id : B
  def nodes : Stream[Tree[A,B]]

  /**
    * Find node by id
    *
    * @param searchId - Search Id
    * @return
    */
  def findNodeById(searchId: B) : Option[Tree[A,B]] = {
    def searchNodes(nds : Stream[Tree[A,B]]) : Option[Tree[A,B]] = {
      nds.headOption.map(n => n.findNodeById(searchId) match {
        case tt@Some(ttree) => tt
        case None => searchNodes(nds.tail)
      }).getOrElse(None)
    }

    if (searchId.equals(id)) Some(this) else searchNodes(nodes)
  }

  /**
    * Get all nodes in the tree
    * @return
    */
  def getDescendants() : List[Tree[A,B]] = {
    def getNodes(nds : Stream[Tree[A,B]]) : List[Tree[A,B]] = nds.headOption.map(tt => {
      tt :: tt.getDescendants() ++ getNodes(nds.tail)
    }).getOrElse(List[Tree[A,B]]())

    getNodes(nodes)
  }

  /**
    * TODO - implement Tree[A,B] equality
    * @param that - Tree[A,B] instance to compare with this instance
    * @return
    */
  def equals(that: Tree[A,B]) = ???
}

/**
  * Tree Operations on Tree[A,B]
  * @tparam A - data parameter type
  * @tparam B - id parameter type
  */
trait TreeOps[A,B] {

  type T <: Tree[TTag,UUID]
  //def prependNode[T](node : => T) : T
  //def prependNodes[T](nodes : => List[T]) : T

  /**
    * Serialise a taxonomy tree to csv
    * @return
    */
  def serialiseTree() : String

  /**
    * Serialise a taxonomy tree to json
    * @return
    */
  def writeJson() : JsValue
}


/**
  * Companion object for creating Tree[A,B] objects
  */
object Tree {
  /**
    * Tree factory
    *
    * @param dataP - data parameter
    * @param idP - id parameter
    * @param nodesP - nodes parameter
    * @tparam A - data parameter type
    * @tparam B - id parameter type
    * @return
    */
  def apply[A, B](dataP : A, idP: B, nodesP : Stream[Tree[A,B]] = Stream[Tree[A,B]]() ) = new Tree[A,B] {
    lazy val data = dataP
    lazy val id = idP
    lazy val nodes = nodesP
  }
}

