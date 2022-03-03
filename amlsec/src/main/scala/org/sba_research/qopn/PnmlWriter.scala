package org.sba_research.qopn

import scalaxb.DataRecord
import scalaxb.DataRecord.{__BigDecimalXMLFormat, __StringXMLFormat}
import www.pnml.org.version2009.grammar.pnml.{Annotationgraphicsu46content, Edgegraphicsu46content, Httpu58u47u47wwwu46pnmlu46orgu47versionu452009u47grammaru47ptnet, Net, Nodegraphicsu46content, Nodeu46contentSequence, Nonnegativeintegerlabelu46content, Page, Placeu46labelsSequence, Pnml, Simpletextlabelu46content, Simpletextu46contentSequence, Type, Arc => Ar, Offset => Offs, Place => Pl, Position => Pos, Transition => Trans}
import xmlprotocol._
import scala.xml.XML

object PnmlWriter {


  private def getPnml(pn: PetriNet): Pnml = {

    val netName = pn.name
    val netId = pn.id
    val pageId = "g-1"

    def getPnmlName(): DataRecord[Simpletextlabelu46content] =
      DataRecord(
        None,
        Some("name"),
        Simpletextlabelu46content(
          List(
            DataRecord(
              None,
              None,
              Simpletextu46contentSequence(Some(netName))
            )
          )
        )
      )


    def getNetAtts(): Map[String, DataRecord[Any]] =
      Map(
        "@id" -> DataRecord[String](
          None,
          None,
          netId
        ),
        "@type" -> DataRecord[Type](
          None,
          None,
          Httpu58u47u47wwwu46pnmlu46orgu47versionu452009u47grammaru47ptnet
        ),
      )

    def getPage(): DataRecord[Page] =
      DataRecord(
        None,
        Some("page"),
        Page(
          pageoption = List(
            pn.transitions.toList.map { case (_, t: Transition) => getTransition(t) },
            pn.places.toList.map { case (_, p: Place) => getPlace(p) },
            pn.arcs.toList.map { case (_, a: Arc) => getArc(a) }
          ).flatten,
          attributes = Map(
            "@id" -> DataRecord[String](
              None,
              None,
              pageId
            )
          )
        )
      )

    def getArc(arc: Arc): DataRecord[Ar] = {

      def getGraphics(): Option[DataRecord[Edgegraphicsu46content]] = {

        def getPosition(): List[DataRecord[Pos]] = arc.position.map { pos =>
          DataRecord[Pos](
            None,
            Some("position"),
            Pos(Map("@x" -> DataRecord[BigDecimal](pos.x), "@y" -> DataRecord[BigDecimal](pos.y)))
          )
        }

        if (arc.position.nonEmpty) {
          Some(
            DataRecord[Edgegraphicsu46content](
              None,
              Some("graphics"),
              Edgegraphicsu46content(
                edgegraphicsu46contentoption = getPosition
              )
            )
          )
        } else None
      }

      DataRecord[Ar](
        None,
        Some("arc"),
        Ar(
          arcoption = List(
            getGraphics
          ).flatten,
          attributes = Map(
            "@id" -> DataRecord[String](
              None,
              None,
              arc.id
            ),
            "@source" -> DataRecord[String](
              None,
              None,
              arc.source.id
            ),
            "@target" -> DataRecord[String](
              None,
              None,
              arc.target.id
            )
          )
        )
      )
    }

    def getTransition(transition: Transition): DataRecord[Trans] = {
      DataRecord[Trans](
        None,
        Some("transition"),
        Trans(
          transitionoption = getNodeOption(transition),
          attributes = getNodeId(transition)
        )
      )
    }

    def getPlace(place: Place): DataRecord[Pl] = {

      def getInitialMarking(): List[DataRecord[Placeu46labelsSequence]] = (pn.marking.tokens.lift(pn.marking.places.indexOf(place)) map { case Token(v) =>
        if (v > 0)
          List(
            DataRecord[Placeu46labelsSequence](
              None,
              None,
              Placeu46labelsSequence(initialMarking =
                Some(
                  Nonnegativeintegerlabelu46content(nonnegativeintegerlabelu46contentoption =
                    List(
                      DataRecord[String](
                        None,
                        Some("text"),
                        v.toString
                      )
                    )
                  )
                )
              )
            )
          )
        else List.empty
      }).getOrElse(List.empty)

      DataRecord[Pl](
        None,
        Some("place"),
        Pl(
          placeoption = getNodeOption(place) ::: getInitialMarking(),
          attributes = getNodeId(place)
        )
      )
    }

    def getNodeId(node: Node) = Map(
      "@id" -> DataRecord[String](
        None,
        None,
        node.id
      )
    )

    def getNodeOption(node: Node): List[DataRecord[Product]] = {

      def getOffset(): Option[DataRecord[Annotationgraphicsu46content]] = node.offset map { o =>
        DataRecord[Annotationgraphicsu46content](
          None,
          Some("graphics"),
          Annotationgraphicsu46content(offset = Offs(Map("@x" -> DataRecord[BigDecimal](o.x), "@y" -> DataRecord[BigDecimal](o.y))))
        )
      }

      def getPosition(): Option[DataRecord[Nodeu46contentSequence]] = node.position map { p =>
        DataRecord[Nodeu46contentSequence](
          None,
          None,
          Nodeu46contentSequence(graphics =
            Some(
              Nodegraphicsu46content(nodegraphicsu46contentableoption =
                List(
                  DataRecord[Pos](
                    None,
                    Some("position"),
                    Pos(Map("@x" -> DataRecord[BigDecimal](p.x), "@y" -> DataRecord[BigDecimal](p.y)))
                  )
                )
              )
            )
          )
        )
      }

      List(
        Some(
          DataRecord[Simpletextlabelu46content](
            None,
            Some("name"),
            Simpletextlabelu46content(
              simpletextlabelu46contentoption = List(
                Some(
                  DataRecord[Simpletextu46contentSequence](
                    None,
                    None,
                    Simpletextu46contentSequence(Some(node.name))
                  )
                ),
                getOffset()
              ).flatten
            )
          )
        ),
        getPosition()
      ).flatten

    }

    Pnml(
      net = List(
        Net(
          netoption = List(
            getPnmlName(),
            getPage()
          ),
          attributes = getNetAtts()
        )
      )
    )

  }

  def apply(filePath: String, pn: PetriNet): Unit = {

    val pnml = getPnml(pn)

    val doc = scalaxb.toXML[Pnml](
      obj = pnml,
      namespace = Some("http://www.pnml.org/version-2009/grammar/pnml"),
      elementLabel = "pnml",
      scope = defaultScope
    )

    // max width: 80 chars, indent: 2 spaces
    val printer = new scala.xml.PrettyPrinter(80, 2)

    doc.headOption foreach { d =>
      XML.save(filePath, XML.loadString(printer.format(d)))
    }

  }

}
