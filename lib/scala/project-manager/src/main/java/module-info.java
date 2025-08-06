import org.enso.logging.config.LoggerSetup;
import org.enso.logging.service.LoggingServiceFactory;

module org.enso.project.manager {
  requires org.apache.commons.io;
  requires org.apache.commons.lang3;
  requires commons.cli;

  requires org.enso.akka.wrapper;
  requires org.enso.distribution;
  requires org.enso.engine.common;
  requires org.enso.json.rpc.server;
  // For pureconfig
  requires org.enso.language.server.deps.wrapper;
  requires org.enso.logging.config;
  requires org.enso.logging.service;
  requires org.enso.logging.utils.akka;
  requires org.enso.os.environment;
  requires org.enso.scala.wrapper;
  requires org.enso.runtime.version.manager;
  requires org.enso.version.output;
  requires org.enso.zio.wrapper;
  requires org.slf4j;

  uses LoggerSetup;
  uses LoggingServiceFactory;
}