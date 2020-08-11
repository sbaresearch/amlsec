package org.sba_research.worker

import akka.actor.DeadLetter
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object SystemListener {
  def apply(): Behavior[Any] =
    Behaviors.setup { ctx =>
      Behaviors.receiveMessage {
        case deadLetter: DeadLetter =>
          ctx.log.info("Dead letter: [sender={}, recipient={}, message={}]", deadLetter.sender, deadLetter.recipient, deadLetter.message)
          Behaviors.unhandled

      }
    }
}
