package org.enso.compiler.dump.service;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

/**
 * A service provider interface to implement to handler dumping of {@link Module} and {@link
 * Expression} IRs. Contains methods that create and deal with {@link org.enso.compiler.core.IR}
 * dumpers. Don't call directly. Use {@link IRDumper} which contains the API for perform the
 * dumping.
 *
 * @param <Dump> type to use for grouping of dumpings
 */
public abstract class IRDumpFactoryService<Dump> {
  public static final String SYSTEM_PROP = "enso.compiler.dumpIr";
  public static final String DEFAULT_DUMP_DIR = "ir-dumps";

  protected abstract Dump create(String moduleName);

  protected abstract void shutdown();

  /**
   * Dumps module IR.
   *
   * @param group
   * @param src what to dump
   */
  protected abstract void dumpModule(Dump group, IRSource<Module> src);

  /**
   * Dumps a single expression IR.
   *
   * @param group
   * @param src
   */
  protected abstract void dumpExpression(Dump group, IRSource<Expression> src);

  /**
   * Close and flush all the underlying resources. There will be no more dumps for the module after
   * this call.
   *
   * @param group
   */
  protected abstract void close(Dump group);
}
