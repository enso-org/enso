package org.enso.compiler.dump.service;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

/** Main entry point to dumping of IR graphs. */
public abstract class IRDumper {
  IRDumper() {}

  /**
   * Creates new dumping group.
   *
   * @param name name of the dumping group
   * @return
   */
  public static IRDumper create(String name) {
    return Impl.create(name, IRDumpSingleton.DEFAULT);
  }

  /**
   * Dumps provide source into this group.
   *
   * @param src info about the data to dump
   */
  public abstract void dumpModule(IRSource<Module> src);

  /**
   * Dumps provide source into this group.
   *
   * @param src info about the data to dump
   */
  public abstract void dumpExpression(IRSource<Expression> src);

  /**
   * Close and flush all the underlying resources. There will be no more dumps for the module after
   * this call.
   */
  public abstract void close();

  /**
   * Open class to hold service and a dumper.
   *
   * @param <D> the type shared by service and dumper.
   */
  private static final class Impl<D> extends IRDumper {
    final IRDumpFactoryService<D> service;
    final D dumper;

    private Impl(IRDumpFactoryService<D> service, D dumper) {
      this.service = service;
      this.dumper = dumper;
    }

    static <D> IRDumper create(String name, IRDumpFactoryService<D> factory) {
      return new Impl<>(factory, factory.create(name));
    }

    @Override
    public void close() {
      service.close(dumper);
    }

    @Override
    public void dumpModule(IRSource<Module> src) {
      service.dumpModule(dumper, src);
    }

    @Override
    public void dumpExpression(IRSource<Expression> src) {
      service.dumpExpression(dumper, src);
    }
  }
}
