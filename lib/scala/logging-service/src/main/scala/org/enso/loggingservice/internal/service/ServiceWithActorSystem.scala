package org.enso.loggingservice.internal.service

import akka.actor.ActorSystem
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.enso.loggingservice.internal.InternalLogger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

trait ServiceWithActorSystem extends Service {
  protected def actorSystemName: String
  implicit protected val actorSystem: ActorSystem =
    ServiceWithActorSystem.initializeActorSystemForLoggingService(
      actorSystemName
    )

  protected def terminateUser(): Future[_]

  abstract override def terminate(): Unit = {
    import actorSystem.dispatcher
    val termination = terminateUser().map(_ => {
      actorSystem.terminate()
    })
    try {
      Await.result(termination, 3.seconds)
    } catch {
      case _: concurrent.TimeoutException =>
        InternalLogger.error("The actor system did not terminate in time.")
    } finally {
      super.terminate()
    }
  }
}

object ServiceWithActorSystem {
  def initializeActorSystemForLoggingService(name: String): ActorSystem = {
    import scala.jdk.CollectionConverters._
    val loggers: java.lang.Iterable[String] =
      Seq("akka.event.Logging$StandardOutLogger").asJava

    val config = ConfigFactory
      .load()
      .withValue("akka.loggers", ConfigValueFactory.fromAnyRef(loggers))
      .withValue(
        "akka.logging-filter",
        ConfigValueFactory.fromAnyRef("akka.event.DefaultLoggingFilter")
      )
    ActorSystem(
      name,
      config,
      classLoader =
        classOf[Server].getClassLoader // Note [Actor System Class Loader]
    )
  }
}
