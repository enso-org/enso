package org.enso.compiler.pass.lint;

import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.pass.IRPass;
import scala.collection.immutable.Seq;

/**
 * A lint compiler pass that checks for shadowed types and methods.
 * If shadowing is detected, a warning is attached to the IR.
 */
public final class ShadowedIdentifiersLint implements IRPass {
  public static final ShadowedIdentifiersLint INSTANCE = new ShadowedIdentifiersLint();
  private UUID uuid;

  @Override
  public UUID key() {
    return uuid;
  }

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    return null;
  }

  @Override
  public Seq<IRPass> invalidatedPasses() {
    return null;
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    if (!moduleContext.compilerConfig().warningsEnabled()) {
      return ir;
    }
    return null;
  }

  /**
   * Not supported for this pass.
   */
  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }
}
