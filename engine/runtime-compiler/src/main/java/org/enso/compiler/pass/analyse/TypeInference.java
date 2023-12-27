package org.enso.compiler.pass.analyse;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.InlineContext$;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.desugar.ComplexType$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import scala.Function1;
import scala.Option;
import scala.PartialFunction;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;

import java.util.List;
import java.util.UUID;

public final class TypeInference implements IRPass {
  public static final TypeInference INSTANCE = new TypeInference();
  private UUID uuid;

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
    List<IRPass> passes = List.of(
        BindingAnalysis$.MODULE$,
        ComplexType$.MODULE$,
        TypeNames$.MODULE$,
        TypeSignatures$.MODULE$
    );
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRPass> invalidatedPasses() {
    return (Seq<IRPass>) Seq$.MODULE$.empty();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    System.out.println("TypeInference.runModule: " + moduleContext.getName());
    return ir.mapExpressions(
        (expression) -> runExpression(expression, new InlineContext(
            moduleContext,
            moduleContext.compilerConfig(),
            Option.empty(),
            Option.empty(),
            Option.empty(),
            Option.empty(),
            Option.empty()
        ))
    );
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir.transformExpressions(new PartialFunction<>() {
      @Override
      public boolean isDefinedAt(Expression x) {
        log("TypeInference.isDefinedAt", x);
        return x instanceof Type.Ascription;
      }

      @Override
      public Expression apply(Expression v1) {
        Type.Ascription ascription = (Type.Ascription) v1;
        log("TypeInference.runExpression", ascription);
        return v1;
      }

      private void log(String prefix, Expression expression) {
        String name = expression.getClass().getCanonicalName();
        name = name.substring(name.indexOf("ir.") + 3);
        System.out.println(prefix + ": " + name + " - " + expression.showCode());
      }
    });
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }
}
