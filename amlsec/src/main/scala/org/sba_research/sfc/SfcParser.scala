package org.sba_research.sfc

import www.plcopen.org.xml.tc60201.Project
import xmlprotocol.`package`.Wwwplcopenorgxmltc60201_ProjectFormat

import scala.xml.XML

case class SfcParsingError(message: String)

object SfcParser {

  def apply(sfcFilePath: String): Either[SfcParsingError, Project] = {

    val doc = XML.loadFile(sfcFilePath)

    scalaxb.fromXMLEither[Project](doc) match {
      case Right(p) => Right(p)
      case Left(errMsg) => Left(SfcParsingError(errMsg))
    }

  }

}
