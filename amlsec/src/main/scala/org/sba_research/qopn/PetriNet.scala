package org.sba_research.qopn

import scala.collection.immutable.SortedMap

case class PetriNet(
                     id: String,
                     name: String,
                     marking: Marking,
                     places: SortedMap[String, Place],
                     transitions: SortedMap[String, Transition],
                     arcs: SortedMap[(String, String), Arc],
                     // Node A -> List of nodes that have an outgoing arc pointing toward node A
                     presetNodes: Map[String, List[Node]],
                     // Node A -> List of nodes that have an incoming arc starting from node A
                     postsetNodes: Map[String, List[Node]],
                     // Node A -> ((source, target) -> arc where target is node A)
                     presetArcs: Map[String, Map[(String, String), Arc]],
                     // Node A -> ((source, target) -> arc where source is node A)
                     postsetArcs: Map[String, Map[(String, String), Arc]]
                   )

trait Element {
  def id: String
}

trait Node extends Element {
  def id: String

  def name: String

  def position: Option[Position]

  def offset: Option[Offset]
}

case class Place(
                  id: String,
                  name: String,
                  offset: Option[Offset] = None,
                  position: Option[Position] = None
                ) extends Node

case class Transition(
                       id: String,
                       name: String,
                       offset: Option[Offset] = None,
                       position: Option[Position] = None
                     ) extends Node

case class Arc(
                id: String,
                source: Node,
                target: Node,
                weight: Int,
                position: List[Position] = List.empty
              )

case class Marking(places: List[Place] = List.empty, tokens: List[Token] = List.empty)

case class Token(v: Long)

case class Position(x: Int, y: Int)

case class Offset(x: Int, y: Int)

object PetriNet {

  def apply(id: String, name: String, marking: Marking, places: SortedMap[String, Place], transitions: SortedMap[String, Transition], arcs: SortedMap[(String, String), Arc]): PetriNet = {

    case class PresetsPostsets(presetNodes: Map[String, List[Node]] = Map.empty,
                               postsetNodes: Map[String, List[Node]] = Map.empty,
                               presetArcs: Map[String, Map[(String, String), Arc]] = Map.empty,
                               postsetArcs: Map[String, Map[(String, String), Arc]] = Map.empty)

    def getPresetsPostsets(a: List[Arc]): Option[PresetsPostsets] = {
      val r = PresetsPostsets()
      LazyList.unfold((r, a)) { case (res, aas) =>
        aas.headOption.map { aa =>
          // Get list of current preset nodes
          val currPresetNodes: List[Node] = res.presetNodes.getOrElse(aa.target.id, List.empty)
          val updatedPresetNodes: Map[String, List[Node]] = res.presetNodes.++(Map(aa.target.id -> (aa.source :: currPresetNodes)))
          // Get list of current postset nodes
          val currPostsetNodes: List[Node] = res.postsetNodes.getOrElse(aa.source.id, List.empty)
          val updatedPostsetNodes: Map[String, List[Node]] = res.postsetNodes.++(Map(aa.source.id -> (aa.target :: currPostsetNodes)))
          // Get list of current preset arcs
          val currPresetArcs: Map[(String, String), Arc] = res.presetArcs.getOrElse(aa.target.id, Map.empty)
          val updatedPresetArcs: Map[String, Map[(String, String), Arc]] = res.presetArcs.++(Map(aa.target.id -> currPresetArcs.++(Map((aa.source.id, aa.target.id) -> aa))))
          // Get list of current postset arcs
          val currPostsetArcs: Map[(String, String), Arc] = res.postsetArcs.getOrElse(aa.source.id, Map.empty)
          val updatedPostsetArcs: Map[String, Map[(String, String), Arc]] = res.postsetArcs.++(Map(aa.source.id -> currPostsetArcs.++(Map((aa.source.id, aa.target.id) -> aa))))

          val updatedResult = r.copy(
            presetNodes = updatedPresetNodes, postsetNodes = updatedPostsetNodes,
            presetArcs = updatedPresetArcs, postsetArcs = updatedPostsetArcs
          )
          updatedResult -> (updatedResult, aas.tail)
        }
      }.lastOption
    }

    val presetsPostsets = getPresetsPostsets(arcs.values.toList)

    PetriNet(
      id = id,
      name = name,
      marking = marking,
      places = places,
      transitions = transitions,
      arcs = arcs,
      presetNodes = presetsPostsets.map(_.presetNodes).getOrElse(Map.empty),
      postsetNodes = presetsPostsets.map(_.postsetNodes).getOrElse(Map.empty),
      presetArcs = presetsPostsets.map(_.presetArcs).getOrElse(Map.empty),
      postsetArcs = presetsPostsets.map(_.postsetArcs).getOrElse(Map.empty),
    )
  }

  def updateUsing(
                   petriNet: PetriNet, id: Option[String] = None,
                   name: Option[String] = None, marking: Option[Marking] = None,
                   places: Option[SortedMap[String, Place]] = None, transitions: Option[SortedMap[String, Transition]] = None,
                   arcs: Option[SortedMap[(String, String), Arc]] = None): PetriNet =

    PetriNet.apply(
      id = id.getOrElse(petriNet.id),
      name = name.getOrElse(petriNet.name),
      marking = marking.getOrElse(petriNet.marking),
      places = places.getOrElse(petriNet.places),
      transitions = transitions.getOrElse(petriNet.transitions),
      arcs = arcs.getOrElse(petriNet.arcs)
    )


}
