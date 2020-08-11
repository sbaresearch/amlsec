package org.sba_research.utils

import java.util.function.Predicate

import com.typesafe.scalalogging.Logger
import org.topbraid.shacl.engine.Shape
import org.topbraid.shacl.model.SHShape

object ShapeFilterByUri {
  def apply(rootShapes: List[Shape], shapeUri: String): ShapeFilterByUri = new ShapeFilterByUri(rootShapes, shapeUri)
}

class ShapeFilterByUri(rootShapes: List[Shape], shapeUri: String) extends Predicate[SHShape] {
  val logger = Logger(getClass)

  /**
    * Tests whether the SHACL shape should be included for the model validation.
    * This method is used to filter out all other shapes in the file, except the one with the matching org.sba_research.utils.ShapeFilterByUri#shapeUri.
    *
    * @param t the SHACL shape being tested
    * @return `true` if the SHACL shape should be included for the model validation
    */
  override def test(t: SHShape): Boolean = {
    val shapeUnderTestIsRootShape = rootShapes.exists { s =>
      Option(s.getShapeResource) match {
        case Some(res) => res.getURI == t.getURI
        case None => false
      }
    }
    if (t.asNode().isURI && shapeUnderTestIsRootShape) { // We only want to exclude root shapes (not shapes that are actually referenced by other shapes)
      shapeUri.split("#").toList.lift(0).map { ns =>
        if (t.getNameSpace == ns.concat("#")) {
          logger.debug(s"Applying shape filter: ${t.asNode().getURI} == $shapeUri: ${t.asNode().getURI == shapeUri}")
          t.asNode().getURI == shapeUri
        } else true // Include SHACL shape, because it's not one of our engineering/security rules
      }.getOrElse(true)
    } else true
  }
}
