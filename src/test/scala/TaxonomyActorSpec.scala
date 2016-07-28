package scala

import java.util.UUID

import akka.actor._
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import model.TaxonomyActor._
import model.{Taxonomy, TTag, TaxonomyActor, TaxonomyNode}
import org.scalatest.{BeforeAndAfterEach, BeforeAndAfterAll, FlatSpecLike, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._
/**
  * Created by edem on 05/07/16.
  */
class TaxonomyActorSpec(_system: ActorSystem) extends TestKit(_system) with FlatSpecLike with Matchers
  with BeforeAndAfterEach with BeforeAndAfterAll with ImplicitSender {

  def this() = this(ActorSystem("TaxonomyActorSpec"))

  var root: ActorRef = _

  override def beforeEach: Unit = root = system.actorOf(Props[TaxonomyActor])
  override def afterEach: Unit = root ! PoisonPill
  override def afterAll: Unit = system.terminate()

  "We" can "create a root node actor" in {

    root ! GetTagName(requester=testActor, opId=1)
    expectMsg(TagName(opId=1, tagName="Categories" ))

    root ! GetNodes(requester=testActor, opId=2)
    expectMsg(Nodes(opId=2, Stream[ActorRef]()))
  }
  "we" should "be able to add a child node" in {

    val nodes = List(system.actorOf(TaxonomyNode.props(TTag.simpleTag("Pop"))))

    root ! PrependNodes(requester=testActor, opId=1, nodes=nodes)
    expectMsg(OpSuccess(opId=1))

    root ! GetNodes(requester=testActor, opId=2)
    expectMsg(Nodes(opId=2, nodes.toStream))
  }
  "We" should "be able to retrieve a root node's id" in {

    root ! GetNodeId(requester=testActor, opId=1)
    expectMsgClass(100 millis, classOf[NodeId])
  }
  "We" should "be able to retrieve the tNode of a childless root" in {

    root ! GetTNode(requester=testActor, opId=1)
    val tNode = receiveOne(100 millis).asInstanceOf[Taxonomy]
    println("tNode = "+tNode)
    tNode.nodes should (be theSameInstanceAs Stream[Taxonomy]())
  }
  "We" should "be able to retrieve the tNode of root with child nodes" in {

    val nodes = List(system.actorOf(TaxonomyNode.props(TTag.simpleTag("Pop"))))
    root ! PrependNodes(requester=testActor, opId=1, nodes=nodes)
    expectMsg(OpSuccess(opId=1))

    root ! GetTNode(requester=testActor, opId=2)
    val tNode = receiveOne(100 millis).asInstanceOf[Taxonomy]
    println("tNode = "+tNode)
    tNode.nodes shouldNot (be theSameInstanceAs Stream[Taxonomy]())
  }
  "We" should "be able to get the root node's id via Find By Id" in {

    root ! GetNodeId(requester=testActor, opId=1)
    val node = receiveOne(100 millis).asInstanceOf[NodeId]

    implicit val timeout = Timeout(100 millis) // needed for `?` below
    val as = (system.actorSelection("/user/*/taxonomy_root") ? Identify(1)).mapTo[ActorIdentity]
    Await.ready(as, 100.millis)

    root ! FindNodeById(requester=testActor, opId=2, node.id)
    expectMsg(NodeFoundById(opId=2,node.id, as.value.get.get.getRef))

    val anyId = UUID.randomUUID()
    root ! FindNodeById(requester=testActor, opId=2, id=anyId)
    expectMsg(NodeNotFoundById(opId=2,sId=anyId, nodes=Stream[ActorRef]()))

  }
  "We" should "be able to get a child node via Find By Id" in {

    val (popNodeId, showNodeId) = (UUID.randomUUID(), UUID.randomUUID())
    val popNode = system.actorOf(TaxonomyNode.props(TTag.simpleTag("Pop"),Stream[ActorRef](),popNodeId))
    val showNode = system.actorOf(TaxonomyNode.props(TTag.simpleTag("Show"),Stream[ActorRef](),showNodeId))

    showNode ! PrependNodes(requester=testActor, opId=1, nodes=List(popNode))
    expectMsg(OpSuccess(opId=1))

    root ! PrependNodes(requester=testActor, opId=2, nodes=List(showNode))
    expectMsg(OpSuccess(opId=2))

    root ! FindNodeById(requester=testActor, opId=3, showNodeId)
    expectMsg(NodeFoundById(opId=3, showNodeId, showNode))

    root ! FindNodeById(requester=testActor, opId=4, popNodeId)
    expectMsg(NodeFoundById(opId=4, popNodeId, popNode))
  }
  "We" should "Retrieve all sibling children of a node" in {

    val (popNodeId, showNodeId) = (UUID.randomUUID(), UUID.randomUUID())
    val popNode = system.actorOf(TaxonomyNode.props(TTag.simpleTag("Pop"),Stream[ActorRef](),popNodeId))
    val showNode = system.actorOf(TaxonomyNode.props(TTag.simpleTag("Show"),Stream[ActorRef](),showNodeId))

    root ! PrependNodes(requester=testActor, opId=1, nodes=List(popNode))
    expectMsg(OpSuccess(opId=1))

    root ! PrependNodes(requester=testActor, opId=2, nodes=List(showNode))
    expectMsg(OpSuccess(opId=2))

    root ! GetDescendants(requester=testActor, opId=3)
    expectMsg(Descendants(opId=3, Stream[ActorRef](), List(popNode, showNode)))
  }
  "We" should "Retrieve all the multi-level descendants of a node" in {

    val (popNodeId, showNodeId) = (UUID.randomUUID(), UUID.randomUUID())
    val popNode = system.actorOf(TaxonomyNode.props(TTag.simpleTag("Pop"),Stream[ActorRef](),popNodeId))
    val showNode = system.actorOf(TaxonomyNode.props(TTag.simpleTag("Show"),Stream[ActorRef](),showNodeId))

    showNode ! PrependNodes(requester=testActor, opId=1, nodes=List(popNode))
    expectMsg(OpSuccess(opId=1))

    root ! PrependNodes(requester=testActor, opId=2, nodes=List(showNode))
    expectMsg(OpSuccess(opId=2))

    root ! GetDescendants(requester=testActor, opId=3)
    expectMsg(Descendants(opId=3, Stream[ActorRef](), List(popNode, showNode)))
  }
  "We" should "Retrieve all the nodes with a particular tag" in {

    val (popNodeTag1,popNodeTag2, showNodeTag) = (TTag.simpleTag("Pop"), TTag.simpleTag("Pop"), TTag.simpleTag("Show"))
    val popNode1 = system.actorOf(TaxonomyNode.props(popNodeTag1,Stream[ActorRef]()))
    val popNode2 = system.actorOf(TaxonomyNode.props(popNodeTag2,Stream[ActorRef]()))
    val showNode = system.actorOf(TaxonomyNode.props(showNodeTag,Stream[ActorRef]()))

    showNode ! PrependNodes(requester=testActor, opId=1, nodes=List(popNode2))
    expectMsg(OpSuccess(opId=1))

    root ! PrependNodes(requester=testActor, opId=2, nodes=List(showNode))
    expectMsg(OpSuccess(opId=2))

    root ! PrependNodes(requester=testActor, opId=3, nodes=List(popNode1))
    expectMsg(OpSuccess(opId=3))

    root ! GetDescendants(requester=testActor, opId=4, "Show")
    expectMsg(Descendants(opId=4, Stream[ActorRef](), List(showNode), "Show"))

    root ! GetDescendants(requester=testActor, opId=5, "Pop")
    expectMsg(Descendants(opId=5, Stream[ActorRef](), List(popNode2, popNode1), "Pop"))
  }
  "We" can "serialise a taxonomy tree to CSV format" in {

    val (popNodeId1, popNodeId2, showNodeId) = (UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
    val (popNodeTag1, popNodeTag2, showNodeTag) = (TTag.simpleTag("Pop"), TTag.simpleTag("Pop"), TTag.simpleTag("Show"))
    val popNode1 = system.actorOf(TaxonomyNode.props(popNodeTag1, Stream[ActorRef](), popNodeId1))
    val popNode2 = system.actorOf(TaxonomyNode.props(popNodeTag2, Stream[ActorRef](), popNodeId2))
    val showNode = system.actorOf(TaxonomyNode.props(showNodeTag, Stream[ActorRef](), showNodeId))

    root ! GetNodeId(requester=testActor, opId=1)
    val rootId = receiveOne(100 millis).asInstanceOf[NodeId].id

    // serialise a node with no sub-nodes
    root ! Serialise(requester=testActor, opId=2)
    expectMsg(Serialised(opId=2, nodes = Stream[ActorRef](), msg="{Categories,"+rootId+",en_GB,Categories}"))

    showNode ! PrependNodes(requester=testActor, opId=3, nodes=List(popNode2))
    expectMsg(OpSuccess(opId=3))

    root ! PrependNodes(requester=testActor, opId=4, nodes=List(showNode))
    expectMsg(OpSuccess(opId=4))

    //serialise a node one nested sub-node
    root ! Serialise(requester=testActor, opId=5)
    expectMsg(Serialised(opId=5, nodes = Stream[ActorRef](), msg="{Categories,"+rootId+",en_GB,Categories,{Show,"+showNodeId+
      ",en_GB,Show,{Pop,"+popNodeId2+",en_GB,Pop}}}"))

    root ! PrependNodes(requester=testActor, opId=6, nodes=List(popNode1))
    expectMsg(OpSuccess(opId=6))

    // serialise a node with sub-node siblings
    root ! Serialise(requester=testActor, opId=7)  //
    expectMsg(Serialised(opId=7, nodes = Stream[ActorRef](), msg="{Categories,"+rootId+",en_GB,Categories,{" +
      "Pop,"+popNodeId1+",en_GB,Pop},{Show,"+showNodeId+",en_GB,Show,{Pop,"+popNodeId2+",en_GB,Pop}}}"))

    root ! Serialise(requester=testActor, opId=8)
    val sMsg = receiveOne(100 millis).asInstanceOf[Serialised].msg
    println("sMsg = "+sMsg)
  }
}