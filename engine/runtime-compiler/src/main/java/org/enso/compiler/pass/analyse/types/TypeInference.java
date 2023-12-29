package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ConstantsNames;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.analyse.AliasAnalysis;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.IntStream;

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

    var inferredType = getInferredType(mappedIr);
    if (inferredType != null) {
      log("inferred type", mappedIr, inferredType.type().toString());
    } else {
      log("inferred type", mappedIr, "NONE");
    }
    return mappedIr;
  }

  private void processTypeAscription(Expression ir) {
    Optional<TypeSignatures.Signature> ascribedSignature =
        getOptionalMetadata(ir, TypeSignatures$.MODULE$, TypeSignatures.Signature.class);
    if (ascribedSignature.isPresent()) {
      TypeSignatures.Signature s = ascribedSignature.get();
      log("type signature", ir, s.signature().showCode());
      TypeRepresentation t = resolveTypeExpression(s.signature());
      if (t != null) {
        setInferredType(ir, new InferredType(t));
      }
    }
  }

  private void processTypePropagation(Expression ir) {
    switch (ir) {
      case Name.Literal l -> processName(l);
      case Application.Force f -> {
        var innerType = getInferredType(f.target());
        if (innerType != null) {
          setInferredType(f, innerType);
        }
      }
      case Application.Prefix p -> {
        // TODO for later - check function return type somehow, first for local functions, later global ones
      }
      case Expression.Binding b -> {
        // TODO propagate the type into scope somehow?
      }
      case Expression.Block b -> {
        var innerType = getInferredType(b.returnValue());
        if (innerType != null) {
          setInferredType(b, innerType);
        }
      }
      case Function.Lambda f -> {
        var type = buildLambdaType(f);
        if (type != null) {
          setInferredType(f, type);
        }
      }
      case Literal l -> processLiteral(l);
      default -> {
        log("type propagation", ir, "UNKNOWN: " + ir.getClass().getCanonicalName());
      }
    }
  }

  private void processName(Name.Literal literalName) {
    // TODO need to reproduce IrToTruffle::processName logic
    // TODO first iteration - just find the local bindings

//    AliasAnalysis.Info.Occurrence occurrence =
//        getMetadata(literalName, AliasAnalysis$.MODULE$, AliasAnalysis.Info.Occurrence.class);
    Optional<BindingsMap.Resolution> global =
        getOptionalMetadata(literalName, GlobalNames$.MODULE$, BindingsMap.Resolution.class);

    // TODO somehow correlate the occurrence with an argument or a local binding
    // TODO how to get here type ascriptions from arguments?? do I need some state when traversing?
    Object inLocalScope = null;
    if (inLocalScope != null) {
      log("processName", literalName, "local scope TODO");
    } else if (global.isPresent()) {
      BindingsMap.ResolvedName resolution = global.get().target();
      processGlobalName(literalName, resolution);
    } else if (literalName.name().equals(ConstantsNames.FROM_MEMBER)) {
      log("processName", literalName, "from conversion - currently unsupported");
    } else {
      // TODO these will be used for member method calls `x.foo`
      log("processName", literalName, "unresolved symbol - TODO");
    }
  }

  private void processGlobalName(Name.Literal literalName, BindingsMap.ResolvedName resolution) {
    // TODO this does not work because My_Type.Singleton is actually an Application of UnresolvedSymbol<Singleton> on ResolvedType<My_Type>, it's not a ResolvedConstructor
    if (resolution instanceof BindingsMap.ResolvedConstructor ctor) {
      // TODO when do these appear??
      log("processGlobalName", literalName, "RESOLVED CONTRUCTOR");

      // TODO we should be able to get the types of each field and add them here
      // Currently we just fall back to Any
      var arguments = IntStream.range(0, ctor.cons().arity()).mapToObj((i) -> TypeRepresentation.ANY).toList();
      var resultType = new TypeRepresentation.AtomType(ctor.tpe().qualifiedName().toString());
      var constructorFunctionType = TypeRepresentation.buildFunction(arguments, resultType);
      setInferredType(literalName, new InferredType(constructorFunctionType));
    } else {
      log("processGlobalName", literalName, "global scope reference to " + resolution + " - currently global inference is unsupported");
    }
  }

  private void processLiteral(Literal literal) {
    TypeRepresentation type = switch (literal) {
      case Literal.Number number -> number.isFractional() ? TypeRepresentation.FLOAT : TypeRepresentation.INTEGER;
      case Literal.Text text -> TypeRepresentation.TEXT;
      // This branch is needed only because Java is unable to infer that the match is exhaustive
      default ->
          throw new IllegalStateException("Impossible - unknown literal type: " + literal.getClass().getCanonicalName());
    };
    setInferredType(literal, new InferredType(type));
  }

  /**
   * Builds the type of a lambda, based on available type information of its parts.
   * <p>
   * The return type is inferred based on the body, and expected argument types are based on type ascriptions of these
   * arguments (currently no upwards propagation of constraints yet). Even if the types are not known, we may fall back
   * to a default unknown type, but we may at least infer the minimum arity of the function.
   */
  private InferredType buildLambdaType(Function.Lambda f) {
    scala.collection.immutable.List<TypeRepresentation> argTypesScala =
        f.arguments()
            .filter((arg) -> !(arg.name() instanceof Name.Self))
            .map((arg) -> {
                  if (arg.ascribedType().isDefined()) {
                    Expression t = arg.ascribedType().get();
                    return resolveTypeExpression(t);
                  } else {
                    return TypeRepresentation.UNKNOWN;
                  }
                }
            );

    InferredType inferredReturnType = getInferredType(f.body());

    if (inferredReturnType == null && argTypesScala.isEmpty()) {
      // If the return type is unknown and we have no arguments, we do not infer anything useful - so we withdraw.
      return null;
    }

    TypeRepresentation returnType =
        inferredReturnType == null ? TypeRepresentation.ANY : inferredReturnType.type();

    TypeRepresentation arrowType = TypeRepresentation.buildFunction(
        CollectionConverters.asJava(argTypesScala),
        returnType
    );
    return new InferredType(arrowType);
  }

  private void setInferredType(Expression expression, InferredType type) {
    Objects.requireNonNull(type, "type must not be null");
    expression.passData().update(this, type);
  }

  private InferredType getInferredType(Expression expression) {
    Option<ProcessingPass.Metadata> r = expression.passData().get(this);
    if (r.isDefined()) {
      return (InferredType) r.get();
    } else {
      return null;
    }
  }

  private void log(String prefix, Expression expression) {
    log(prefix, expression, null);
  }

  private TypeRepresentation resolveTypeExpression(Expression type) {
    return switch (type) {
      case Name.Literal name -> {
        Optional<BindingsMap.Resolution> resolutionOptional =
            getOptionalMetadata(name, TypeNames$.MODULE$, BindingsMap.Resolution.class);
        if (resolutionOptional.isPresent()) {
          BindingsMap.ResolvedName target = resolutionOptional.get().target();
          yield new TypeRepresentation.AtomType(target.qualifiedName().toString());
        } else {
          log("resolveTypeExpression", type, "Missing TypeName resolution metadata");
          yield TypeRepresentation.UNKNOWN;
        }
      }

      case Set.Union union -> {
        var operands = union.operands().map(this::resolveTypeExpression);
        yield new TypeRepresentation.SumType(CollectionConverters.asJava(operands));
      }

      case Set.Intersection intersection -> {
        var lhs = resolveTypeExpression(intersection.left());
        var rhs = resolveTypeExpression(intersection.right());
        yield new TypeRepresentation.IntersectionType(List.of(lhs, rhs));
      }

      // We could extract more info form function, but we deliberately do not.
      // This is because our ascriptions (x : A -> B) only check (x.is_a Function), so all we get is that it is a
      // function with at least one argument (and we can't even tell its full arity).
      // Later, we could extract this as some kind of secondary metadata, but currently we do not because it could be
      // misleading - this property is _not_ guaranteed at runtime as other ascriptions are. Functions not matching
      // this type will still be allowed. That's why we return the more generic type that covers everything that the
      // check actually lets through.
      case Type.Function function -> new TypeRepresentation.ArrowType(
          TypeRepresentation.UNKNOWN,
          TypeRepresentation.ANY
      );

      // We just ignore the error part for now as it's not really checked anywhere.
      case Type.Error error -> resolveTypeExpression(error.typed());

      default -> {
        log("resolveTypeExpression", type, "UNKNOWN BRANCH");
        yield TypeRepresentation.UNKNOWN;
      }
    };
  }

  private <T> Optional<T> getOptionalMetadata(IR ir, IRPass pass, Class<T> expectedType) {
    Option<ProcessingPass.Metadata> option = ir.passData().get(pass);
    if (option.isDefined()) {
      try {
        return Optional.of(expectedType.cast(option.get()));
      } catch (ClassCastException exception) {
        throw new IllegalStateException("Unexpected metadata type " + option.get().getClass().getCanonicalName() + " " +
            "for " + pass, exception);
      }
    } else {
      return Optional.empty();
    }
  }

  private <T> T getMetadata(IR ir, IRPass pass, Class<T> expectedType) {
    Optional<T> optional = getOptionalMetadata(ir, pass, expectedType);
    if (optional.isEmpty()) {
      throw new IllegalStateException("Missing expected " + pass + " metadata for " + ir + ".");
    }

    return optional.get();
  }

  private void log(String prefix, Expression expression, String suffix) {
    String name = expression.getClass().getCanonicalName();
    name = name.substring(name.indexOf("ir.") + 3);

    String suffixStr = suffix == null ? "" : " ==> " + suffix;
    System.out.println(prefix + ": " + name + " - " + expression.showCode() + suffixStr);
  }
}
