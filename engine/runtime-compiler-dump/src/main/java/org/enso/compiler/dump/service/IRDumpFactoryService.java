package org.enso.compiler.dump.service;

/** A service that creates {@link org.enso.compiler.core.IR} dumpers for modules. */
public interface IRDumpFactoryService {
  static final IRDumpFactoryService DEFAULT = IRDumpSingleton.DEFAULT;

  String SYSTEM_PROP = "enso.compiler.dumpIr";
  String DEFAULT_DUMP_DIR = "ir-dumps";

  IRDumper create(String moduleName);

  void shutdown();
}
