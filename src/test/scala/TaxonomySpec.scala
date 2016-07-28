package scala

import java.util.UUID

import model.{TTag, Taxonomy}
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.{JsError, JsSuccess}

/**
  * Created by edem on 01/07/16.
  */
class TaxonomySpec extends FlatSpec with Matchers {
  "We" can "create a root node" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    root.data.name should be ("Categories")
  }
  "A tree node" should "have a unique id" in {
    val popTree1 = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val popTree2 = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    popTree1.id should not equal popTree2.id
  }
  "we" should "be able to add a child node" in {
    val parentTree = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
      .prependNode(Taxonomy(TTag("Pop", List(("en_GB", "Pop")))))

    parentTree.nodes.headOption should not be None
  }
  "We" should "be able to retrieve a root node's id" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    root.findNodeById(root.id).get.id should equal(root.id)
  }
  "We" should "be able to retrieve a child nodes by id" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = Taxonomy(TTag("Show", List(("en_GB", "Show")))).prependNode(popTree)
    val parentTree = root.prependNode(showTree)

    parentTree.findNodeById(popTree.id).get.id should equal (popTree.id)
    parentTree.findNodeById(showTree.id).get.id should equal (showTree.id)
  }
  "We" should "Retrieve all sibling children of a node" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = Taxonomy(TTag("Show", List(("en_GB", "Show"))))
    val parentTree = root.prependNode(popTree).prependNode(showTree)
    parentTree.getDescendants().toSet should equal(Set(popTree, showTree))
  }
  "We" should "Retrieve all the multi-level descendants of a node" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = Taxonomy(TTag("Show", List(("en_GB", "Show")))).prependNode(popTree)
    val parentTree = root.prependNode(showTree)
    parentTree.getDescendants().toSet should equal(Set(popTree, showTree))
  }
  "We" should "Retrieve all the nodes with a particular tag" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree1 = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val popTree2 = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = Taxonomy(TTag("Show", List(("en_GB", "Show")))).prependNode(popTree2)
    val parentTree = root.prependNode(showTree).prependNode(popTree1)

    //println("parentTree.findByTag(\"Pop\") = "+parentTree.findNodesByTag("Pop"))
    parentTree.findNodesByTag("Show").toSet should equal(Set(showTree))
    parentTree.findNodesByTag("Pop").toSet should equal(Set(popTree1, popTree2))
  }
  "We" can "serialise a taxonomy tree to CSV format" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree1 = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val popTree2 = Taxonomy(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = Taxonomy(TTag("Show", List(("en_GB", "Show")))).prependNode(popTree2)
    val parentTree = root.prependNode(showTree).prependNode(popTree1)

    val serialisedParenTree = parentTree.serialiseTree()

    val joinedTrees = "{Categories,"+parentTree.id+",en_GB,Categories,{" +
      "Pop,"+popTree1.id+",en_GB,Pop},{Show,"+showTree.id+",en_GB,Show,{Pop,"+popTree2.id+",en_GB,Pop}}}"

    val joinedSerialisedTrees = "{Categories," + parentTree.id + ",en_GB,Categories," +popTree1.serialiseTree() +
      "," + showTree.serialiseTree() +"}"

    //println("serialisedParenTree = " + serialisedParenTree)
    serialisedParenTree should equal(joinedTrees)

    //println("joined tree.serialiseCSV = "+ joinedSerialisedTrees)
    serialisedParenTree should equal(joinedSerialisedTrees)
  }
  "We" can "deserialise CSV format to a taxonomy tree" in {
    val csvData = "{Categories,"+UUID.randomUUID()+",en_GB,Categories,{Pop,"+UUID.randomUUID()+",en_GB,Pop},{Show," +
      UUID.randomUUID()+",en_GB,Show,{Pop,"+UUID.randomUUID()+",en_GB,Pop}}}"
    //println(Taxonomy.readCsv(csvData))
    Taxonomy.readCsv(csvData).serialiseTree() should equal(csvData)
    an [Exception] should be thrownBy { Taxonomy.readCsv("{Categories,"+UUID.randomUUID()+",en_GB,Categories,") }
    an [Exception] should be thrownBy { Taxonomy.readCsv("{Categories,"+UUID.randomUUID()+",en_GB,Categories}}") }
    an [Exception] should be thrownBy { Taxonomy.readCsv("{Categories,"+UUID.randomUUID()+",en_GB,Categories},}") }
    an [Exception] should be thrownBy { Taxonomy.readCsv("{Categories,"+UUID.randomUUID()+",en_GB}") }
    an [Exception] should be thrownBy { Taxonomy.readCsv("{Categories,"+UUID.randomUUID()+"}") }
  }
  "we" can "serialise a taxonomy tree to Json" in {
    println(Taxonomy(TTag("Categories", List(("en_GB", "Categories")))).writeJson())
    println(Taxonomy.readCsv("{Categories,"+UUID.randomUUID()+",en_GB,Categories,{Pop,"+UUID.randomUUID()+",en_GB,Pop},{Show," +
      UUID.randomUUID()+",en_GB,Show,{Pop,"+UUID.randomUUID()+",en_GB,Pop}}}").writeJson())

  }
  "we" can "read a taxonomy json to a taxonomy tree" in {
    val root = Taxonomy(TTag("Categories", List(("en_GB", "Categories"))))
    val jStr = root.writeJson().toString()
    Taxonomy.readJson(jStr) match {
      case JsSuccess(t,p) => t.writeJson() should equal(root.writeJson())
      case JsError(e) => fail(e.toString())
    }
  }
}
