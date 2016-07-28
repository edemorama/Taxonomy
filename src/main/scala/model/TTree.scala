package model

import java.util.UUID

/**
  * Taxonomy Tree with tag data and tree nodes
  *
  * @param tag - tag data
  * @param nodes - multiple sub-nodes of the tree
  * @param id - node identifier
  */
class TTree(val tag : TTag, val nodes : Stream[TTree] = Stream[TTree](), val id: UUID = UUID.randomUUID()) {
  /**
    *
    * Add Node to head of Nodes returning a new Tree
    *
    * @param child - node to add
    * @return  TTree
    */
  def addNode(child: TTree) = TTree(tag, child #:: nodes)

  /**
    * Find node by id
    *
    * @param searchId - Search Id
    * @return Option[TTree]
    */
  def findNodeById(searchId: UUID) : Option[TTree] = {
    def searchNodes(nds : Stream[TTree]) : Option[TTree] = {
      nds.headOption.map(_.findNodeById(searchId) match {
        case tt@Some(ttree) => tt
        case None => searchNodes(nds.tail)
      }).getOrElse(None)
    }

    if (searchId.equals(id)) Some(this) else searchNodes(nodes)
  }

  /**
    * Get all nodes in the tree
    *
    * @return  List[TTree]
    */
  def getDescendants : List[TTree] = {
    def getNodes(nds : Stream[TTree]) : List[TTree] = nds.headOption.map(tt => {
      tt :: tt.getDescendants ++ getNodes(nds.tail)
    }).getOrElse(List[TTree]())

    getNodes(nodes)
  }

  /**
    * Find all nodes with a given tag
    *
    * @param name - tag name of nodes to find
    * @return  List[TTree]
    */
  def findNodesByTag(name: String) : List[TTree] = {
    def findTagBy(nds : Stream[TTree]) : List[TTree] = nds match {
      case t #:: ts  =>  t.findNodesByTag(name) ++ findTagBy(ts)
      case Stream.Empty => List[TTree]()
    }
    val tagCh = if (name == tag.name) this :: findTagBy(nodes) else findTagBy(nodes)
    //println("name = "+name +", tag = "+tag.name +", descendants = " +tagCh )
    tagCh
  }

  /**
    *  Serialise a taxonomy tree of e.g (Categories -> Pop, Show -> Pop) to
    *  "{Categories,UUID1,en_GB,Categories,{Pop,UUID2,en_GB,Pop},{Show,UUID3,en_GB,Show,{Pop,UUID4,en_GB,Pop}}}"
    */
  def serialiseTree() : String = {
    val sNodes = nodes.map(_.serialiseTree()).mkString(",")
    val sTag = tag.name + "," + id + tag.locales.map(tr => "," + tr._1 + "," + tr._2).mkString(",")

    if (sNodes == "") "{" + sTag + "}" else "{" + sTag  + "," + sNodes + "}"
  }

  def serialiseTree2() : String = {
    "{" + tag.locales.foldLeft(tag.name +","+ id)((acc,tr) =>  acc + "," + tr._1 + "," + tr._2) +
      nodes.foldLeft("")((acc, nd) => acc + "," + nd.serialiseTree2()) + "}"  // serialise tag + serialise nodes
  }

}

/**
  * Companion Object which can deserialise a csv taxonomy data to a TTree
  */
object TTree {
  def apply(tag : TTag, nodes : Stream[TTree] = Stream[TTree](), id: UUID = UUID.randomUUID()) = new TTree(tag, nodes, id)

  /**
    * Deserialise csv taxonomy data to a taxonomy tree
    * e.g.{Categories,UUID1,en_GB,Categories,{Pop,UUID2,en_GB,Pop},{Show,UUID3,en_GB,Show,{Pop,UUID4,en_GB,Pop}}}" to
    *  TTree(Categories -> Pop, Show -> Pop)
    *
    * @param csvData
    * @return TTree
    */
  def apply(csvData : String) : TTree = ???
}

