import org.scalatest._
import model.{TTag, TTree}

class TTreeSpec extends FlatSpec with Matchers {
  "We" should "be able to create a root node" in {
    val root = TTree(TTag("Categories", List(("en_GB", "Categories"))))
    root.tag.name should be ("Categories")
  }

  "A tree node" should "have a unique id" in {
    val popTree1 = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val popTree2 = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    popTree1.id should not equal popTree2.id
  }

  "we" should "be able to add a child node" in {
    val parentTree = TTree(TTag("Categories", List(("en_GB", "Categories"))))
      .addNode(TTree(TTag("Pop", List(("en_GB", "Pop")))))

    parentTree.nodes.headOption should not be None
  }

  "We" should "be able to retrieve a root node's id" in {
    val root = TTree(TTag("Categories", List(("en_GB", "Categories"))))
    root.findNodeById(root.id).get.id should equal(root.id)
  }

  "We" should "be able to retrieve child nodes by id" in {
    val root = TTree(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = TTree(TTag("Show", List(("en_GB", "Show")))).addNode(popTree)
    val parentTree = root.addNode(showTree)

    parentTree.findNodeById(popTree.id).get.id should equal (popTree.id)
    parentTree.findNodeById(showTree.id).get.id should equal (showTree.id)
  }

  "We" should "Retrieve all the children of a node" in {
    val root = TTree(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = TTree(TTag("Show", List(("en_GB", "Show"))))
    val parentTree = root.addNode(popTree).addNode(showTree)
    parentTree.getDescendants.toSet should equal(Set(popTree, showTree))
  }

  "We" should "Retrieve all the multi-level descendants of a node" in {
    val root = TTree(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = TTree(TTag("Show", List(("en_GB", "Show")))).addNode(popTree)
    val parentTree = root.addNode(showTree)
    parentTree.getDescendants.toSet should equal(Set(popTree, showTree))
  }

  "We" should "Retrieve all the nodes with a particular tag" in {
    val root = TTree(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree1 = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val popTree2 = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = TTree(TTag("Show", List(("en_GB", "Show")))).addNode(popTree2)
    val parentTree = root.addNode(showTree).addNode(popTree1)

    val findShow = parentTree.findNodesByTag("Show")
    val findPop =  parentTree.findNodesByTag("Pop")


    println("parentTree.findByTag(\"Show\") = "+findShow)
    println("parentTree.findByTag(\"Show\").toSet = "+findShow.toSet)

    println("parentTree.findByTag(\"Pop\") = "+findPop)
    println("parentTree.findByTag(\"Pop\").toSet = "+findPop.toSet)

    findShow.toSet should equal(Set(showTree))
    findPop.toSet should equal(Set(popTree1, popTree2))
  }

  "We" can "serialise a taxonomy tree to CSV format" in {
    val root = TTree(TTag("Categories", List(("en_GB", "Categories"))))
    val popTree1 = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val popTree2 = TTree(TTag("Pop", List(("en_GB", "Pop"))))
    val showTree = TTree(TTag("Show", List(("en_GB", "Show")))).addNode(popTree2)
    val parentTree = root.addNode(showTree).addNode(popTree1)

    val serialisedParenTree = parentTree.serialiseTree()

    val joinedTrees = "{Categories,"+parentTree.id+",en_GB,Categories,{" +
      "Pop,"+popTree1.id+",en_GB,Pop},{Show,"+showTree.id+",en_GB,Show,{Pop,"+popTree2.id+",en_GB,Pop}}}"

    val joinedSerialisedTrees = "{Categories," + parentTree.id + ",en_GB,Categories," +popTree1.serialiseTree() +
      "," + showTree.serialiseTree() +"}"

    println("serialisedParenTree = " + serialisedParenTree)
    serialisedParenTree should equal(joinedTrees)

    println("joined tree.serialiseCSV = "+ joinedSerialisedTrees)
    serialisedParenTree should equal(joinedSerialisedTrees)

  }

  "We" can "deserialise CSV format to a taxonomy tree" in {
    val csvData = "{Categories,UUID1,en_GB,Categories,{Pop,UUID2,en_GB,Pop},{Show,UUID3,en_GB,Show,{Pop,UUID4,en_GB,Pop}}}"
    val taxonomyTree = TTree(csvData)

    taxonomyTree.serialiseTree() should equal(csvData)
  }

}
