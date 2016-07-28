package model

import java.util.UUID

import akka.actor.{ActorLogging, Actor, Props, ActorRef}
import akka.event.LoggingReceive
import akka.util.Timeout
import akka.pattern.{ask, pipe}
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.Success

/**
  * Created by edem on 26/07/16.
  */

/**
  * TaxonomyNode Companion object with actor creation props
  */
object TaxonomyNode {
  def props(data : TTag, nodes: Stream[ActorRef] = Stream[ActorRef](), id: UUID = UUID.randomUUID() ) =
    Props(classOf[TaxonomyNode], data, nodes, id)
}

/**
  * A Taxonomy Rose tree node with a data tag and a stream of actor references to other nodes
  *
  * @param data - Tag data
  * @param nodes - Stream of nodes
  * @param id - node identifier
  */
class TaxonomyNode(val data: TTag, var nodes : Stream[ActorRef] = Stream[ActorRef](),
                   val id: UUID = UUID.randomUUID() ) extends Actor with ActorLogging {
  import TaxonomyActor._

  var tNode: Taxonomy = Taxonomy(data, Stream[Taxonomy](), id)

  import context.dispatcher
  implicit val timeout = Timeout(100 millis) // needed for `?` below

  /**
    * Standard akka message processing
    *
    * @return
    */
  def receive: Receive = LoggingReceive {
    case GetNodeId(r, o) => r ! NodeId(o, id)
    case GetNodes(r, o) => r ! Nodes(o, nodes)

    case GetTNode(r, o, t) if t == ReqType.TELL => getTNode(r, o)
    case GetTNode(r, o, t) if t == ReqType.ASK => getTNode(sender(), o) //sender() needed here when replying to an ask

    case UpdateTNode(r, o) => updateTNode

    case GetTagName(r, o) => r ! TagName(o, data.name)

    case PrependNodes(r, o, n) => prependNodes(r, o, n)

    case FindNodeById(r, oId, sId) => searchNodesById(r, oId, sId, nodes)
    case SearchNodesById(r, oId, sId, nds) => searchNodesById(r, oId, sId, nds)

    case GetDescendants(r, oId,tNm) => getDescendants(r, oId, tNm, nodes, List[ActorRef]())
    case GetNodesDescendants(r, oId, tNm, nds, rnds) => getDescendants(r, oId, tNm, nds, rnds)

    case Serialise(r, opId) =>  serialise(r, opId, nodes, "")
    case SerialiseNodes(r, opId, nds, msg) => serialise(r, opId, nds, msg)
  }
  /**
    * Message processing state when processing the nodes
    *
    * @param requester - Actor to send the processing results to
    * @return
    */
  def  processingNodes(requester: ActorRef): Receive = LoggingReceive {
    case NodeNotFoundById(oId,sId, nds) =>
      nds match {
        case n #:: ns => n ! SearchNodesById(self, oId, sId, ns)
        case _ =>
          context.become(receive)
          requester ! NodeNotFoundById(oId,sId, nds)
      }

    case NodeFoundById(oId,sId,n) =>
      context.become(receive)
      requester !  NodeFoundById(oId,sId,n)

    case Descendants(oId, nds, rnds, tName) =>
      nds match {
        case n #:: ns => n ! GetNodesDescendants(self, oId, tName, ns, rnds)
        case _ =>
          context.become(receive)
          requester ! Descendants(oId, nds, rnds, tName)
      }
    case Serialised(oId, nds, msg) =>
      nds match {
        case n #:: ns => n ! SerialiseNodes(self, oId, ns, msg)
        case _ =>
          context.become(receive)
          requester ! Serialised(oId, nds, msg + "}")
      }
  }
  /**
    * Find a node with a given id
    *
    * @param requester - Actor to send the processing results to
    * @param opId - Operation Id
    * @param sId - Search Node Id
    * @param nds - Stream of nodes to search
    */
  def searchNodesById(requester: ActorRef, opId: Int, sId: UUID, nds: Stream[ActorRef]) = {
    if (sId.equals(id)) requester ! NodeFoundById(opId, sId, self)
    else {
      nodes match {
        case n #:: ns =>
          context.become(processingNodes(requester))
          n ! SearchNodesById(self, opId, sId, ns)
        case _ => requester ! NodeNotFoundById(opId, sId, nds)
      }
    }
  }

  /**
    * Retrieve all nodes and subnodes
    *
    * @param requester - Actor to send the processing results to
    * @param opId  - Operation Id
    * @param tName - tag name to search for
    * @return
    */
  def getDescendants(requester: ActorRef, opId: Int, tName: String, nds: Stream[ActorRef], rnds: List[ActorRef]) = {
    val rNodes = if (data.name == tName || (tName == "" && data.name != RootTagName)) self :: rnds else rnds
    nodes match {
      case n #:: ns =>
        context.become(processingNodes(requester))
        n ! GetNodesDescendants(self, opId, tName, ns, rNodes)
      case _ => requester ! Descendants(opId, nds, rNodes, tName)
    }
  }

  /**
    * Serialise a node ands all its sub-nodes
    *
    * @param requester - Actor to send the processing results to
    * @param opId - Operation Id
    * @param nds - Stream of nodes to serialise
    * @param msg - serialised message in progress
    * @return
    */
  def serialise(requester: ActorRef, opId: Int, nds: Stream[ActorRef], msg: String) = {
    val sTag = data.name + "," + id + data.locales.map(tr => "," + tr._1 + "," + tr._2).mkString(",")
    val rMsg = if (msg == "") "{" + sTag  else msg + ",{" + sTag

    nodes match {
      case n #:: ns =>
        context.become(processingNodes(requester))
        n ! SerialiseNodes(self, opId, ns, rMsg)
      case _ =>
        requester ! Serialised(opId, nds, rMsg + "}")
    }
  }
  /**
    * Add a node by prepending to the stream of sub-nodes
    *
    * @param requester - Actor to send the processing results to
    * @param opId - Operation Id
    * @param nds  - Stream of nodes to process
    */
  def prependNodes(requester: ActorRef, opId: Int, nds : => List[ActorRef]) = {
    nodes = nds.toStream ++ nodes
    requester ! OpSuccess(opId)
  }

  /**
    * Set the taxonomy value on the tNode variable
    * @return
    */
  def updateTNode = {
    Future.sequence(nodes.map(a => a ? GetTNode(self, 0, ReqType.ASK)).map(_.mapTo[Taxonomy])).andThen({
      case Success(nds) =>
        tNode = tNode.prependNodes(nds.toList)
        //println("tNode inside updateTNode = "+tNode)
      case r@_ => println("Don't know why I am not in the Success "+r)
    })
  }
  /**
    * Get the tNode from this actor node, setting this value if it is empty
    *
    * @param requester - Actor to send the tNode to
    * @param opId - Operation Id
    * @return
    */
  def getTNode(requester: ActorRef, opId: Int) = {
    println("(tNode.nodes.isEmpty && !nodes.isEmpty = "+(tNode.nodes.isEmpty && nodes.nonEmpty))
    if (tNode.nodes.isEmpty && nodes.nonEmpty) {
      updateTNode.map(nds => tNode).pipeTo(requester)
    } else { requester ! tNode}
  }
}
