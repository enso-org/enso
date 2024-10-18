package org.enso.compiler.dump;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.IRProcessingPass;
import scala.collection.immutable.Seq;

/** A pass that just dumps IR to the local {@code ir-dumps} directory. See {@link IRDumper}. */
public class IRDumperPass implements IRPass {
  public static final IRDumperPass INSTANCE = new IRDumperPass();

  private IRDumperPass() {}

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
    var irDumpsDir = Path.of(IRDumper.DEFAULT_DUMP_DIR);
    if (!irDumpsDir.toFile().exists()) {
      try {
        Files.createDirectory(irDumpsDir);
      } catch (IOException e) {
        throw new IllegalStateException(e);
      }
    }
    var modName = moduleContext.getName().toString();
    var irPath = irDumpsDir.resolve(modName + ".dot");
    var irDumper = IRDumper.fromPath(irPath);
    irDumper.dump(ir);
    System.out.println("IR dumped to " + irPath);
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
