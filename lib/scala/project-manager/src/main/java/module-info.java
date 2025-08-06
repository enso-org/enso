import org.enso.logging.config.LoggerSetup;
import org.enso.logging.service.LoggingServiceFactory;

module org.enso.project.manager {
  requires org.apache.commons.io;
  requires org.apache.commons.lang3;
  requires commons.cli;

  requires org.enso.akka.wrapper;
  requires org.enso.distribution;
  requires org.enso.editions;
  requires org.enso.editions.updater;
  requires org.enso.engine.common;
  requires org.enso.json.rpc.server;
  // For pureconfig
  requires org.enso.language.server.deps.wrapper;
  requires org.enso.logging.config;
  requires org.enso.logging.service;
  requires org.enso.logging.utils.akka;
  requires org.enso.logging.utils;
  requires org.enso.os.environment;
  requires org.enso.pkg;
  requires org.enso.runtime.version.manager;
  requires org.enso.scala.wrapper;
  requires org.enso.semver;
  requires org.enso.task.progress.notifications;
  requires org.enso.version.output;
  requires org.enso.zio.wrapper;
  requires org.slf4j;

  // This package is used by io.circe decoder, which accesses it via reflection.
  opens org.enso.projectmanager.data;
  exports org.enso.projectmanager.model;

  uses LoggerSetup;
  uses LoggingServiceFactory;
}