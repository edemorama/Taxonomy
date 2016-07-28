package model

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._
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

    case MakeTree(r, o, t) if t == ReqType.TELL => makeTree(r, o)
    case MakeTree(r, o, t) if t == ReqType.ASK => makeTree(sender(), o) //sender() needed here when replying to an ask

    case GetTagName(r, o) => r ! TagName(o, data.name)

    case PrependNodes(r, o, n) => prependNodes(r, o, n)

    case FindNodeById(r, o, sId) => searchNodesById(r, o, sId, nodes)

    case SearchNodesById(r, o, sId, nds) => searchNodesById(r, o, sId, nds)

    case GetDescendants(r, o, tNm, t)  if t == ReqType.TELL => findDescendants(r, o, tNm)
    case GetDescendants(r, o, tNm, t)  if t == ReqType.ASK => findDescendants(sender(), o, tNm)

    case Serialise(r, o, t) if t == ReqType.TELL  => serialise(r, o)
    case Serialise(r, o, t) if t == ReqType.ASK  => serialise(sender(), o)
  }

  /**
    * Message processing state when processing the nodes
    *
    * @param requester - Actor to send the processing results to
    * @return
    */
  def  processingNodes(requester: ActorRef): Receive = LoggingReceive {
    case NodeNotFoundById(oId, sId, nds) =>
      nds match {
        case n #:: ns => n ! SearchNodesById(self, oId, sId, ns)
        case _ =>
          context.become(receive)
          requester ! NodeNotFoundById(oId,sId, nds)
      }

    case NodeFoundById(oId,sId,n) =>
      context.become(receive)
      requester !  NodeFoundById(oId,sId,n)
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
  def findDescendants(requester: ActorRef, opId: Int, tName: Option[String]) = {
    Future.sequence(nodes.map(a => a ? GetDescendants(self, opId, tName, ReqType.ASK)).map(_.mapTo[Descendants])).map(ss => {
      val ssNodes = ss.flatMap(_.rnds).toList
      val rNodes = tName match {
        case None if data.name != rootTagName =>  self :: ssNodes
        case Some(tn) if data.name == tn =>  self :: ssNodes
        case _ => ssNodes
      }
      //println("rNodes = "+rNodes2.mkString)
      Descendants(opId, rNodes, tName)
    }).pipeTo(requester)
  }

  /**
    * Serialise a node ands all its sub-nodes
    *
    * @param requester - Actor to send the processing results to
    * @param opId - Operation Id
    * @return
    */
  def serialise(requester: ActorRef, opId: Int) = {
    val sTag = data.name + "," + id + data.locales.map(tr => "," + tr._1 + "," + tr._2).mkString(",")
    Future.sequence(nodes.map(a => a ? Serialise(self, opId, ReqType.ASK)).map(_.mapTo[Serialised])).map(ss => {
      val msg = "{" + sTag + (if (ss.isEmpty) "}" else "," + ss.map(_.msg).mkString(",") + "}")
      //println("serialse msg = "+msg)
      Serialised(opId, msg)
    }).pipeTo(requester)
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
    * Make a [Taxonomy] node from this actor node
    *
    * @param requester - Actor to send the [Taxonomy] node to
    * @param opId - Operation Id
    * @return
    */
  def makeTree(requester: ActorRef, opId: Int) = {
    Future.sequence(nodes.map(a => a ? MakeTree(self, 0, ReqType.ASK)).map(_.mapTo[Taxonomy])).map(ss => {
      Taxonomy(data, Stream[Taxonomy](), id).prependNodes(ss.toList)
    }).pipeTo(requester)
  }
}
