package org.enso.compiler.dump;

import java.io.File;
import java.util.ServiceLoader;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumpService;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.IRProcessingPass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.collection.immutable.Seq;

/** A pass that just dumps IR to the local {@code ir-dumps} directory. */
public final class IRDumperPass implements IRPass {
  public static final String SYSTEM_PROP = "enso.compiler.dumpIr";
  private final Logger logger = LoggerFactory.getLogger(IRDumperPass.class);
  private final IRDumpService dumpService;

  /**
   * @param dumper Class name for the {@link IRDumpService} to use.
   */
  public IRDumperPass(String dumper) {
    this.dumpService = loadService(dumper);
    if (this.dumpService != null) {
      logger.info("Found IRDumpService: {}", dumper);
    } else {
      logger.error("No IRDumpService found for {}", dumper);
    }
  }

  private static IRDumpService loadService(String implName) {
    var loader = ServiceLoader.load(IRDumpService.class);
    for (IRDumpService service : loader) {
      if (service.getClass().getName().equals(implName)) {
        return service;
      }
    }
    return null;
  }

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    return nil();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    return nil();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    var moduleName = moduleContext.getName().toString();
    if (dumpService != null) {
      var path = moduleContext.module().getPath();
      var file = path == null ? null : new File(path);
      dumpService.dump(ir, moduleName, file);
    }
    return ir;
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  @SuppressWarnings("unchecked")
  private static scala.collection.immutable.List<IRProcessingPass> nil() {
    Object obj = scala.collection.immutable.Nil$.MODULE$;
    return (scala.collection.immutable.List<IRProcessingPass>) obj;
  }
}
