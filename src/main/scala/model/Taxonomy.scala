package model

import java.util.UUID
import play.api.libs.json.{JsResult, Json}
import JsonFormatter._

/**
  * Created by edem on 02/07/16.
  */

/**
  * Taxonomy Tree with tag data and tree nodes
  *
  * @param data - Tag data
  * @param id - node identifier
  * @param nodes - Stream of nodes
  */
case class Taxonomy(val data: TTag, val nodes : Stream[Taxonomy] = Stream[Taxonomy](),
                    val id: UUID = UUID.randomUUID()) extends Tree[TTag,UUID] with TreeOps[TTag,UUID] {
  /**
    * Add a node to the tree's nodes collection returning a new Tree
    * @param node - node to add
    * @return
    */
  def prependNode(node : => Taxonomy) = Taxonomy(data, node #:: nodes, id)

  /**
    * Add a list of nodes to the tree's nodes collection returning a new Tree
    * @param nds - List of nodes to add to Tree
    * @return
    */
  def prependNodes(nds : => List[Taxonomy]) = Taxonomy(data, nds.toStream ++ nodes, id)

  /**
    * Find nodes by Tag
    * @param name - Tag name of nodes to find
    * @return
    */
  def findNodesByTag(name: String) : List[Taxonomy] = {
    def findTagBy(nds : Stream[Taxonomy]) : List[Taxonomy] = nds match {
      case t #:: ts  =>  t.asInstanceOf[Taxonomy].findNodesByTag(name) ++ findTagBy(ts)
      case Stream.Empty => List[Taxonomy]()
    }
    if (name == data.name) this :: findTagBy(nodes) else findTagBy(nodes)
  }

  /**
    * Serialise a taxonomy tree (Categories -> (Pop, Show -> Pop)) to
    *  "{Categories,UUID1,en_GB,Categories,{Pop,UUID2,en_GB,Pop},{Show,UUID3,en_GB,Show,{Pop,UUID4,en_GB,Pop}}}"
    *
    * @return
    */
  override def serialiseTree() : String = {
    val sNodes = nodes.map(n => n.serialiseTree()).mkString(",")
    val sTag = data.name + "," + id + data.locales.map(tr => "," + tr._1 + "," + tr._2).mkString(",")
    if (sNodes == "") "{" + sTag + "}" else "{" + sTag  + "," + sNodes + "}"
  }

  /**
    * Serialise a taxaonomy tree  (Categories -> Pop, Show -> Pop) to JSON format
    * @return
    */
  def writeJson() =  Json.toJson(this)
}

/**
  * Companion object with csv and json deserialisation methods
  */
object Taxonomy {
  /**
    * Deserialise csv taxonomy data to a taxonomy tree
    * e.g.{Categories,UUID1,en_GB,Categories,{Pop,UUID2,en_GB,Pop},{Show,UUID3,en_GB,Show,{Pop,UUID4,en_GB,Pop}}}" to
    *  TTree(Categories -> Pop, Show -> Pop)
    *
    *  Split the taxonomy string at each '{' into a List and foldRight the List creating a node which adds the previous
    *  node as either a child or a sibling. Keep a '{' and '}' count increasing with '}' and decreasing with '{', with
    *  the aim to have 0 count at the end.
    *
    * @param csvData - Serialised string of Taxonomy data
    * @return
    */
  def readCsv(csvData : String) : Taxonomy = {

    def readNode(csv: String) = {
      val csvNode = csv.split("[,}]")
      if(csvNode.length < 4 || csvNode.length % 2 == 1) throw new Exception("incorrect node data structure <"+csvNode+">")

      val tagPart = csvNode.splitAt(2)
      val name_id = tagPart._1
      val locales = tagPart._2.sliding(2,2).toList.foldLeft(List[(String, String)]())((acc, lc) => (lc(0), lc(1)) :: acc )
      val tag = TTag(name_id(0), locales)

      val rbrCount = if(csv.indexOf('}') >= 0) if (csv.last == '}') csv.length() - csv.indexOf('}') else 1 else 0   //count '}'
      (Taxonomy(tag, Stream[Taxonomy](), UUID.fromString(name_id(1))), rbrCount)
    }

    val csvNodes = csvData.split('{').toList.tail
    val lT_brC = csvNodes.foldRight((List[Taxonomy](), 0): (List[Taxonomy], Int)) ((n,acc) => {
      val nodeT = readNode(n)
      (acc._1, nodeT._2) match {
        case (Nil, 0) => throw new Exception("Single Node missing closing <}>")
        case (Nil, k) => (List(nodeT._1), acc._2 + k -1)
        case (nds, 0) => (List(nodeT._1.prependNodes(nds)), acc._2 - 1)
        case (nds, k) => (nodeT._1 :: nds, acc._2 + k -1)
      }
    })

    if (lT_brC._1.length > 1) throw new Exception("We have multiple orphan nodes <"+lT_brC._1+">")
    if (lT_brC._2 != 0) throw new Exception("node data structure has bracket imbalance <"+lT_brC._2+">")

    lT_brC._1.head
  }

  /**
    * Deserialise taxonomy json to a taxonomy tree
    * @param jStr - json serialised taxonomy string
    * @return
    */
  def readJson(jStr: String) : JsResult[Taxonomy] = Json.fromJson[Taxonomy](Json.parse(jStr))
}