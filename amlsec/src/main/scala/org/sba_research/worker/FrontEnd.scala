package org.sba_research.worker

sealed trait Command

case object StartRiskIdentification extends Command

case class Failed(work: Work) extends Command

case class Retry(work: Work) extends Command

case class WorkAccepted(work: Work) extends Command

case class WorkFinished(work: Work, result: WorkResult) extends Command with CborSerializable

sealed trait WorkStatus

case object ToSubmit extends WorkStatus

case object Accepted extends WorkStatus

case object Completed extends WorkStatus