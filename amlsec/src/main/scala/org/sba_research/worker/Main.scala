package org.sba_research.worker

import java.io.File
import java.util.concurrent.CountDownLatch
import akka.actor.typed.ActorSystem
import akka.actor.typed.pubsub.Topic
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.{Cluster, SelfUp, Subscribe}
import akka.persistence.cassandra.testkit.CassandraLauncher
import com.typesafe.config.{Config, ConfigFactory}
import org.sba_research.worker.Mode.Mode

object Mode extends Enumeration {
  type Mode = Value
  val Sec, Qual = Value
}

// cf. https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala
object Main {

  // note that 2551 and 2552 are expected to be seed nodes though, even if
  // the back-end starts at 2000
  val backEndPortRange = 2000 to 2999

  val frontEndPortRange = 3000 to 3999

  def main(args: Array[String]): Unit = {
    (args.headOption, args.lastOption) match {

      case (Some(portString), Some(modeString)) if !portString.equals(modeString) && portString.matches("""\d+""") =>
        val mode = getMode(modeString)
        val port = portString.toInt
        if (backEndPortRange.contains(port)) start(port, "back-end", mode)
        else if (frontEndPortRange.contains(port)) start(port, "front-end", mode)
        else start(port, "worker", mode, args.lift(1).map(_.toInt).getOrElse(1))

      case (Some("cassandra"), _) =>
        startCassandraDatabase()
        println("Started Cassandra, press Ctrl + C to kill")
        new CountDownLatch(1).await()

      case (Some(head), Some(_)) =>
        startClusterInSameJvm(mode = getMode(head))

      case _ =>
        throw new IllegalArgumentException("Invalid mode option. Use either '-s' for AMLsec or '-q' for AMLqual.")

    }
  }

  def getMode(modeString: String): Mode = modeString match {
    case "-s" => Mode.Sec
    case "-q" => Mode.Qual
    case _ => throw new IllegalStateException(s"Invalid mode option $modeString. Use either '-s' for AMLsec or '-q' for AMLqual.")
  }

  def startClusterInSameJvm(mode: Mode): Unit = {
    startCassandraDatabase()
    // two backend nodes
    start(2551, "back-end", mode)
    start(2552, "back-end", mode)
    // one front-end node
    start(3000, "front-end", mode)
    // two worker nodes with two worker actors each
    //start(5001, "worker", 1)
    start(5001, "worker", mode, 2)
    start(5002, "worker", mode, 2)
  }

  def start(port: Int, role: String, mode: Mode, workers: Int = 2): Unit = {
    ActorSystem(
      Behaviors.setup[SelfUp](ctx => {
        val cluster = Cluster(ctx.system)
        cluster.subscriptions ! Subscribe(ctx.self, classOf[SelfUp])
        import akka.actor.typed.scaladsl.adapter._
        val sysListener = ctx.spawn(SystemListener(), "sysListener")
        ctx.system.toClassic.eventStream.subscribe(sysListener.toClassic, classOf[akka.actor.DeadLetter])

        Behaviors.receiveMessage {
          case SelfUp(_) =>
            val resultTopicActor = ctx.spawn(Topic[WorkFinished]("work-result"), "WorkResultTopic")

            ctx.log.info("Node is up")
            if (cluster.selfMember.hasRole("back-end")) {
              WorkManagerSingleton.init(ctx.system, resultTopicActor)
            }
            if (cluster.selfMember.hasRole("front-end")) {
              val workManagerProxy = WorkManagerSingleton.init(ctx.system, resultTopicActor)
              mode match {
                case Mode.Sec => ctx.spawn(FrontEndAMLsec(workManagerProxy, resultTopicActor), "front-end")
                case Mode.Qual => ctx.spawn(FrontEndAMLqual(workManagerProxy, resultTopicActor), "front-end")
              }
            }
            if (cluster.selfMember.hasRole("worker")) {
              (1 to workers).foreach(n => ctx.spawn(Worker(resultTopicActor), s"worker-$n"))
            }
            Behaviors.same
        }
      }),
      "ClusterSystem",
      config(port, role)
    )
  }

  def config(port: Int, role: String): Config =
    ConfigFactory.parseString(
      s"""
      akka.remote.artery.canonical.port=$port
      akka.cluster.roles=[$role]
    """).withFallback(ConfigFactory.load())

  /**
    * To make the sample easier to run we kickstart a Cassandra instance to
    * act as the journal. Cassandra is a great choice of backend for Akka Persistence but
    * in a real application a pre-existing Cassandra cluster should be used.
    */
  def startCassandraDatabase(): Unit = {
    val databaseDirectory = new File("target/cassandra-db")
    CassandraLauncher.start(
      databaseDirectory,
      CassandraLauncher.DefaultTestConfigResource,
      clean = false,
      port = 9042
    )

    // shut the cassandra instance down when the JVM stops
    sys.addShutdownHook {
      CassandraLauncher.stop()
    }
  }

}
