package org.enso.loggingservice.internal.service

import akka.actor.ActorSystem
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.enso.loggingservice.internal.InternalLogger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
  * A mix-in for implementing services that use an Akka [[ActorSystem]].
  */
trait ServiceWithActorSystem extends Service {

  /**
    * Name to use for the [[ActorSystem]].
    */
  protected def actorSystemName: String

  /**
    * The [[ActorSystem]] that can be used by the service.
    */
  implicit protected val actorSystem: ActorSystem =
    initializeActorSystemForLoggingService(actorSystemName)

  /**
    * Initializes an [[ActorSystem]], overriding the default logging settings.
    *
    * The default logger cannot be used, because if it is set to use the logging
    * service, logging service errors could potentially lead to cycles or errors
    * not being displayed if the logging service fails.
    */
  private def initializeActorSystemForLoggingService(
    name: String
  ): ActorSystem = {
    import scala.jdk.CollectionConverters._
    val loggers: java.lang.Iterable[String] =
      Seq("akka.event.Logging$StandardOutLogger").asJava
    akka.event.Logging
    val config = ConfigFactory
      .load()
      .withValue("akka.loggers", ConfigValueFactory.fromAnyRef(loggers))
      .withValue(
        "akka.logging-filter",
        ConfigValueFactory.fromAnyRef("akka.event.DefaultLoggingFilter")
      )
      .withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("WARNING"))
    ActorSystem(
      name,
      config,
      classLoader =
        classOf[Server].getClassLoader // Note [Actor System Class Loader]
    )
  }

  /**
    * Called before terminating the [[ActorSystem]], can be used to handle any
    * actions that should happen before it is terminated.
    *
    * The actor system will wait with its termination until the returned future
    * completes.
    */
  protected def terminateUser(): Future[_]

  /**
    * Waits for up to 3 seconds for the [[terminateUser]] and [[ActorSystem]]
    * termination, then handles any other termination logic.
    */
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

/* Note [Actor System Class Loader]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Without explicitly setting the ClassLoader, the ActorSystem initialization
 * fails (at least if run in `sbt`) with `java.lang.ClassCastException:
 * interface akka.event.LoggingFilter is not assignable from class
 * akka.event.DefaultLoggingFilter` which is most likely caused by the two
 * instances coming from distinct class loaders.
 */
