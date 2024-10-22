package org.enso.compiler.pass.analyse.types;

import java.util.List;
import java.util.Objects;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;

/**
 * The compiler pass implementing the proof of concept of type inference.
 *
 * <p>It implements a basic flow of types through expressions, and reports warnings if situations
 * that surely will lead to runtime errors are detected.
 *
 * <p>This pass is still a work in progress and much of the functionality is not yet implemented.
 *
 * <h2>Type Propagation</h2>
 *
 * The expressions are analyzed from the bottom up, propagating types through the expression tree.
 * The leaf expression types are inferred either based on an inherent type of a literal (e.g. `42`
 * is an Integer), based on an already known type of a referenced binding, or based on type
 * ascriptions which assert the type in the runtime (thus the static analysis pass knows that if the
 * runtime gets past such an ascription, the value must have contained the correct type - otherwise
 * an exception would prevent such block from continuing execution).
 *
 * <p>Whenever a type is inferred for a sub-expression, it is stored as {@link InferredType} in its
 * metadata. This metadata is then checked by the composite expressions to infer their types. For
 * example, the result type of a function or constructor application can be computed based on known
 * signature and known argument types.
 *
 * <p>Currently, the type propagation does not handle module-level bindings nor recursion. This will
 * be addressed in future iterations.
 *
 * <h2>Reporting Warnings</h2>
 *
 * We can easily detect a situation where function application is applied to a non-functional type
 * (e.g. Integer). We know that this will result in `Not_Invokable` error in the runtime. Thus, such
 * code is considered broken and the static analysis pass emits a warning letting the user know.
 *
 * <p>The `Not_Invokable` warning is only emitted if the error is guaranteed to happen at runtime
 * (if the code path is reached). If the inferred type is e.g. `Function | Integer`, that means the
 * error may or may not happen depending on actual value. In such case, at least currently, no
 * warning is emitted, because that could lead to too many false positives.
 *
 * <p>Later, we plan to expand the type compatibility checks to type mismatches in function
 * arguments and type ascriptions. However, to do so we need to first handle type conversions that
 * may make the types compatible, to avoid false positives.
 *
 * @see TypePropagation for more details on the type propagation mechanism
 */
public final class TypeInference implements IRPass {
  public static final TypeInference INSTANCE = new TypeInference();
  private static final Logger logger = LoggerFactory.getLogger(TypeInference.class);
  private final BuiltinTypes builtinTypes = new BuiltinTypes();
  private final TypeResolver typeResolver = new TypeResolver();
  private final TypeCompatibility checker = new TypeCompatibility(builtinTypes);
  private final TypePropagation typePropagation =
      new TypePropagation(typeResolver, checker, builtinTypes) {
        @Override
        protected void encounteredIncompatibleTypes(
            IR relatedIr, TypeRepresentation expected, TypeRepresentation provided) {
          relatedIr
              .getDiagnostics()
              .add(
                  new Warning.TypeMismatch(
                      relatedIr.identifiedLocation(), expected.toString(), provided.toString()));
        }

        @Override
        protected void encounteredInvocationOfNonFunctionType(
            IR relatedIr, TypeRepresentation type) {
          relatedIr
              .getDiagnostics()
              .add(new Warning.NotInvokable(relatedIr.identifiedLocation(), type.toString()));
        }
      };

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(
            BindingAnalysis$.MODULE$,
            GlobalNames$.MODULE$,
            FullyQualifiedNames$.MODULE$,
            TypeNames$.MODULE$,
            Patterns$.MODULE$,
            TypeSignatures$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRProcessingPass> invalidatedPasses() {
    return (Seq<IRProcessingPass>) Seq$.MODULE$.empty();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    ir.bindings()
        .map(
            (def) ->
                switch (def) {
                  case Method.Explicit b -> {
                    TypeRepresentation methodBodyType =
                        typePropagation.tryInferringType(b.body(), LocalBindingsTyping.create());

                    if (methodBodyType != null) {
                      setInferredType(b, methodBodyType);
                    }

                    yield b;
                  }
                  case Definition.Type typ -> typ;
                  default -> {
                    logger.trace("UNEXPECTED definition {}", def.getClass().getCanonicalName());
                    yield def;
                  }
                });

    return ir;
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    TypeRepresentation inferredType =
        typePropagation.tryInferringType(ir, LocalBindingsTyping.create());
    if (inferredType != null) {
      setInferredType(ir, inferredType);
    }

    return ir;
  }

  static void setInferredType(IR ir, TypeRepresentation type) {
    Objects.requireNonNull(type, "type must not be null");
    ir.passData().update(INSTANCE, new InferredType(type));
  }
}
