package org.enso.compiler.pass.analyse;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.InlineContext$;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Application$;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.desugar.ComplexType$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
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
    var ctx = new InlineContext(
        moduleContext,
        moduleContext.compilerConfig(),
        Option.empty(),
        Option.empty(),
        Option.empty(),
        Option.empty(),
        Option.empty()
    );

    System.out.println("TypeInference.runModule: " + moduleContext.getName());
    var mappedBindings = ir.bindings().map((def) -> {
      switch (def) {
        case Method.Explicit b -> {
          System.out.println("\ninside method " + b.methodReference().name());
        }
        default -> {
          System.out.println("\ndefinition " + def.getClass().getCanonicalName() + " - " + def.showCode());
        }
      }
      return def.mapExpressions(
          (expression) -> runExpression(expression, ctx)
      );
    });

    return ir.copy(ir.imports(), ir.exports(), mappedBindings, ir.location(), ir.passData(), ir.diagnostics(), ir.id());
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    // We first run the inner expressions, as most basic inference is propagating types in a bottom-up manner.
    var mappedIr = ir.mapExpressions(
        (expression) -> runExpression(expression, inlineContext)
    );

    processTypeAscription(mappedIr);
    processTypePropagation(mappedIr);

    // TODO check if type was inferred and log type or if none
    log("inferred type", ir);
    return mappedIr;
  }

  private void processTypeAscription(Expression ir) {
    Option<ProcessingPass.Metadata> r = ir.passData().get(TypeSignatures$.MODULE$);
    if (r.isDefined()) {
      // This type would be guaranteed in the Scala typesystem without the cast:
      TypeSignatures.Signature s = (TypeSignatures.Signature) r.get();
      log("type signature", ir, s.signature().showCode());
    }
  }

  private void processTypePropagation(Expression ir) {
    switch (ir) {
      case Name.Literal l -> processName(l);
      case Application.Force f -> {
        // TODO propagate inner type if exists
      }
      case Application.Prefix p -> {
        // TODO for later - check function return type somehow, first for local functions, later global ones
      }
      case Expression.Binding b -> {
        // TODO ?
      }
      case Expression.Block b -> {
        // TODO propagate type from return
        var returnValue = b.returnValue();
      }
      case Function.Lambda f -> {
        // TODO construct a -> type
      }
      case Literal l -> processLiteral(l);
      default -> {
        log("type propagation", ir, "UNKNOWN: "+ir.getClass().getCanonicalName());
      }
    }
  }

  private void processName(Name.Literal literalName) {
    // TODO need to reproduce IrToTruffle::processName logic
    // TODO first iteration - just find the local bindings
  }

  private void processLiteral(Literal literal) {
    switch (literal) {
      case Literal.Number number -> {
        if (number.isFractional()) {
          // TODO add constant type
        } else {
          // TODO add constant type
        }
      }
      case Literal.Text text -> {
        // TODO add constant type
      }

      // This branch is needed only because Java is unable to infer that the match is exhaustive
      default -> throw new IllegalStateException("Impossible - unknown literal type: " + literal.getClass().getCanonicalName());
    }
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  private void log(String prefix, Expression expression) {
    log(prefix, expression, null);
  }

  private void log(String prefix, Expression expression, String suffix) {
    String name = expression.getClass().getCanonicalName();
    name = name.substring(name.indexOf("ir.") + 3);
    String suffixStr = suffix == null ? "" : " --> " + suffix;
    System.out.println(prefix + ": " + name + " - " + expression.showCode() + suffixStr);
  }
}
