import org.enso.runner.common.LanguageServerApi;

module org.enso.language.server {
  requires java.logging;
  requires scala.library;

  requires commons.cli;
  requires flatbuffers.java;
  requires org.apache.commons.io;
  requires org.graalvm.polyglot;
  requires org.eclipse.jgit;
  requires org.slf4j;

  requires org.enso.cli;
  requires org.enso.akka.wrapper;
  requires org.enso.zio.wrapper;
  requires org.enso.distribution;
  requires org.enso.runner.common;
  requires org.enso.engine.common;
  requires org.enso.editions;
  requires org.enso.editions.updater;
  requires org.enso.filewatcher;
  requires org.enso.json.rpc.server;
  requires org.enso.logging.utils;
  requires org.enso.logging.utils.akka;
  requires org.enso.logging.service;
  requires org.enso.lockmanager.server;
  requires org.enso.librarymanager;
  requires org.enso.polyglot.api;
  requires org.enso.pkg;
  requires org.enso.profiling;
  requires org.enso.scala.wrapper;
  requires org.enso.language.server.deps.wrapper;
  requires org.enso.searcher;
  requires org.enso.semver;
  requires org.enso.version.output;
  requires org.enso.text.buffer;
  requires org.enso.task.progress.notifications;
  requires org.enso.ydoc;

  exports org.enso.languageserver.boot;
  exports org.enso.languageserver.filemanager to scala.library;
  exports org.enso.languageserver.runtime to scala.library;
  exports org.enso.languageserver.search to scala.library;

  provides LanguageServerApi with org.enso.languageserver.boot.LanguageServerRunner;
}
