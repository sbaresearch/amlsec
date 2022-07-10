package org.sba_research.qopn

import java.io.File

case class LoLAWriterError(message: String)

case class LoLAWriterResult(pn: PetriNet, path: String)

object LoLAWriter {

  private def toLoLA(pn: PetriNet): List[String] = {

    def fixIdentifier(s: String): String = s.replaceAll("[\\^,;:()\\t\\s\\n\\r\\{\\}]+", "_")

    def getPlaces(): String =
      pn.places.values.toList.map(p => fixIdentifier(p.id)).mkString(", ") ++ ";"

    def getMarkings(): String =
      (pn.marking.places zip pn.marking.tokens).map { case (p, t) =>
        fixIdentifier(p.id) ++ ": " ++ t.v.toString
      }.mkString(",") ++ ";"

    def getTransitions(): List[String] = {

      def getTransition(transition: Transition): String = {

        def getPreset(): String =
          "CONSUME " ++
            pn.presetArcs.get(transition.id)
              .map(m =>
                m.values.toList
                  .map(a => fixIdentifier(a.source.id) ++ ":" ++ a.weight.toString))
              .map(_.mkString(","))
              .getOrElse("") ++
            ";"

        def getPostset(): String =
          "PRODUCE " ++
            pn.postsetArcs.get(transition.id)
              .map(m =>
                m.values.toList
                  .map(a => fixIdentifier(a.target.id) ++ ":" ++ a.weight.toString))
              .map(_.mkString(","))
              .getOrElse("") ++
            ";"

        "TRANSITION " ++ fixIdentifier(transition.id) ++ "\n\t" ++ getPreset() ++ "\n\t" ++ getPostset() ++ "\n"
      }

      pn.transitions.values.toList.map(getTransition)
    }

    List("PLACE", "\t" ++ getPlaces ++ "\n", "MARKING", "\t" ++ getMarkings ++ "\n") ::: getTransitions()
  }

  def apply(filePath: String, pn: PetriNet): Either[LoLAWriterError, LoLAWriterResult] = {
    val f = new File(filePath)
    val p = new java.io.PrintWriter(f)
    try {
      toLoLA(pn).foreach(p.println)
      Right(LoLAWriterResult(pn, filePath))
    }
    catch {
      case e: Exception => Left(LoLAWriterError(e.getMessage))
    }
    finally {
      p.close()
    }
  }

}
