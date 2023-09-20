package org.enso.compiler.pass.analyse;

import java.util.List;
import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.pass.IRPass;
import org.enso.interpreter.util.ScalaConversions;
import scala.collection.convert.AsScalaConverters;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

public class PrivateModuleAnalysis implements IRPass {
  public static final PrivateModuleAnalysis MODULE$ = new PrivateModuleAnalysis();
  private UUID uuid;

  private PrivateModuleAnalysis() {}

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public UUID key() {
    return uuid;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    return ScalaConversions.cons(BindingAnalysis$.MODULE$, ScalaConversions.nil());
  }

  @Override
  public Seq<IRPass> invalidatedPasses() {
    return ScalaConversions.nil();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    System.out.println("Running PrtivateModuleAnalysis");
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
}
