package org.enso.loggingservice.internal.serviceconnection

import akka.actor.ActorSystem
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}

import scala.concurrent.Future

trait ServiceWithActorSystem extends Service {
  protected def actorSystemName: String
  implicit protected val actorSystem: ActorSystem =
    ServiceWithActorSystem.initializeActorSystemForLoggingService(
      actorSystemName
    )

  protected def terminateUser(): Future[_]

  abstract override def terminate(): Unit = {
    import actorSystem.dispatcher
    terminateUser().onComplete(_ => {
      println("stopping AS")
      actorSystem.terminate()
      super.terminate()
    })
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
