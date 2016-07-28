package model

import java.util.UUID

import akka.actor._


/**
  * Created by edem on 04/07/16.
  */
/**
  * Companion object containing actor request and reply messages and their type traits
  */
object TaxonomyActor {
  trait Operation {
    def requester: ActorRef
    def opId: Int
  }

  trait OperationReply {
    def opId: Int
  }

  object ReqType extends Enumeration {
    type ReqType = Value
    val ASK, TELL = Value
  }

  /*
    Root node values
   */
  val RootTagName = "Categories"
  val RootTag = TTag.simpleTag(RootTagName)

  import ReqType._
  /*
    Request messages
  */
  case class GetTagName(requester: ActorRef, opId: Int) extends Operation
  case class GetNodeId(requester: ActorRef, opId: Int) extends Operation
  case class GetNodes(requester: ActorRef, opId: Int) extends Operation
  case class GetTNode(requester: ActorRef, opId: Int, reqType: ReqType = TELL) extends Operation
  case class UpdateTNode(requester: ActorRef, opId: Int) extends Operation
  case class PrependNodes(requester: ActorRef, opId: Int, nodes: List[ActorRef]) extends Operation
  case class FindNodeById(requester: ActorRef, opId: Int, id: UUID) extends Operation
  case class SearchNodesById(requester: ActorRef, opId: Int, sId: UUID, nodes: Stream[ActorRef]) extends Operation
  case class SearchNodesByTag(requester: ActorRef, opId: Int, tagName: String, nodes: Stream[ActorRef]) extends Operation
  case class GetDescendants(requester: ActorRef, opId: Int, tagName: String = "") extends Operation
  case class GetNodesDescendants(requester: ActorRef, opId: Int, tagName: String,
                                 nodes: Stream[ActorRef], resultNodes: List[ActorRef]) extends Operation
  case class Serialise(requester: ActorRef, opId: Int) extends Operation
  case class SerialiseNodes(requester: ActorRef, opId: Int, nodes: Stream[ActorRef], msg: String) extends Operation

  /*
    Reply messages
   */
  case class TagName(opId: Int, tagName: String) extends OperationReply
  case class NodeId(opId: Int, id: UUID) extends OperationReply
  case class Nodes(opId: Int, nodes: Stream[ActorRef]) extends OperationReply
  case class Node(opId: Int, node: Option[ActorRef]) extends OperationReply
  case class OpSuccess(opId: Int) extends OperationReply
  case class NodeNotFoundById(opId: Int, sId: UUID, nodes: Stream[ActorRef]) extends OperationReply
  case class NodeFoundById(opId: Int, sId: UUID, n: ActorRef) extends OperationReply
  case class NodeNotFoundByTag(opId: Int, tNm: String, nodes: Stream[ActorRef]) extends OperationReply
  case class Descendants(opId: Int, nodes: Stream[ActorRef], rnds: List[ActorRef], tNm: String = "") extends OperationReply
  case class Serialised(opId: Int, nodes: Stream[ActorRef], msg: String) extends OperationReply
}

/**
  * Taxonomy Rose tree actor which instantiates the root node and passes incoming messages to it
  */
class TaxonomyActor extends Actor with ActorLogging {
  import TaxonomyActor._
  def createRoot : ActorRef = context.actorOf(TaxonomyNode.props(RootTag), name="taxonomy_root")
  var root = createRoot
  def receive: Receive = { case op: Operation => root ! op }
}
