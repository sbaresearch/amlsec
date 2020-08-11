package org.sba_research.worker

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.pubsub.Topic
import akka.cluster.typed.ClusterSingleton

import scala.concurrent.duration._
import akka.cluster.typed._
import org.sba_research.worker.FrontEnd.WorkFinished
import org.sba_research.worker.WorkManager.Command

// cf. https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala
object WorkManagerSingleton {

  private val singletonName = "work-manager"
  private val singletonRole = "back-end"

  def init(system: ActorSystem[_], resultTopicActor: ActorRef[Topic.Command[WorkFinished]]): ActorRef[Command] = {
    val workTimeout = system.settings.config.getDuration("distributed-workers.work-timeout").getSeconds.seconds

    ClusterSingleton(system).init(
      SingletonActor(WorkManager(workTimeout, resultTopicActor), singletonName)
        .withSettings(ClusterSingletonSettings(system).withRole(singletonRole)))
  }
}
