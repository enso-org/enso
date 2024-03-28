package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.analyse.alias.Info;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.CollectionConverters$;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import static org.enso.compiler.MetadataInteropHelpers.getMetadata;
import static org.enso.compiler.MetadataInteropHelpers.getOptionalMetadata;
import static org.enso.compiler.pass.analyse.types.CommonTypeHelpers.getInferredType;

public final class TypeInference implements IRPass {
  public static final TypeInference INSTANCE = new TypeInference();
  private static final Logger logger = LoggerFactory.getLogger(TypeInference.class);
  private final BuiltinTypes builtinTypes = new BuiltinTypes();
  private final TypeResolver typeResolver = new TypeResolver();
  private final TypeCompatibility checker = new TypeCompatibility(builtinTypes);
  private final TypePropagation typePropagation = new TypePropagation(typeResolver, builtinTypes) {
    @Override
    protected void checkTypeCompatibility(IR relatedIr, TypeRepresentation expected, TypeRepresentation provided) {
      TypeInference.this.checkTypeCompatibility(relatedIr, expected, provided);
    }

    @Override
    protected void encounteredInvocationOfNonFunctionType(IR relatedIr, TypeRepresentation type) {
      relatedIr.diagnostics().add(new Warning.NotInvokable(relatedIr.location(), type.toString()));
    }
  };
  private UUID uuid;

  private void checkTypeCompatibility(IR relatedIr, TypeRepresentation expected, TypeRepresentation provided) {
    TypeCompatibility.Compatibility compatibility = checker.computeTypeCompatibility(expected, provided);
    if (compatibility == TypeCompatibility.Compatibility.NEVER_COMPATIBLE) {
      relatedIr.diagnostics().add(new Warning.TypeMismatch(relatedIr.location(), expected.toString(), provided.toString()));
    }
  }

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
    List<IRPass> passes = List.of(BindingAnalysis$.MODULE$, GlobalNames$.MODULE$, FullyQualifiedNames$.MODULE$, TypeNames$.MODULE$, Patterns$.MODULE$, TypeSignatures$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRPass> invalidatedPasses() {
    return (Seq<IRPass>) Seq$.MODULE$.empty();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    var ctx = new InlineContext(moduleContext, moduleContext.compilerConfig(), Option.empty(), Option.empty(), moduleContext.freshNameSupply(), moduleContext.passConfiguration(), moduleContext.pkgRepo());

    BindingsMap bindingsMap = getMetadata(ir, BindingAnalysis$.MODULE$, BindingsMap.class);
    var mappedBindings = ir.bindings().map((def) -> switch (def) {
      case Method.Explicit b -> {
        var mapped = def.mapExpressions((expression) -> analyzeExpression(expression, LocalBindingsTyping.create()));

        var methodBodyType = getInferredType(b.body());
        if (methodBodyType != null) {
          setInferredType(b, methodBodyType);
        }

        yield mapped;
      }
      case Definition.Type typ -> typ;
      default -> {
        logger.trace("UNEXPECTED definition {}", def.getClass().getCanonicalName());
        yield def;
      }
    });

    return ir.copy(ir.imports(), ir.exports(), mappedBindings, ir.location(), ir.passData(), ir.diagnostics(), ir.id());
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return analyzeExpression(ir, LocalBindingsTyping.create());
  }

  private Expression analyzeExpression(Expression ir, LocalBindingsTyping localBindingsTyping) {
    // We first run the inner expressions, as most basic inference is propagating types in a bottom-up manner.
    var mappedIr = switch (ir) {
      case Function.Lambda lambda -> {
        for (var arg : CollectionConverters$.MODULE$.asJava(lambda.arguments())) {
          if (arg.ascribedType().isDefined()) {
            var type = typeResolver.resolveTypeExpression(arg.ascribedType().get());
            registerBinding(arg, type, localBindingsTyping);
          }
        }
        var newBody = analyzeExpression(lambda.body(), localBindingsTyping);
        yield lambda.copy(lambda.arguments(), newBody, lambda.location(), lambda.canBeTCO(), lambda.passData(), lambda.diagnostics(), lambda.id());
      }
      case Case.Expr caseExpr -> {
        var newScrutinee = analyzeExpression(caseExpr.scrutinee(), localBindingsTyping);
        List<Case.Branch> newBranches = CollectionConverters$.MODULE$.asJava(caseExpr.branches()).stream().map((branch) -> {
          // TODO once we will be implementing type equality constraints*, we will need to copy localBindingsTyping here, to ensure independent typing of branches
          //  (*) (case x of _ : Integer -> e) ==> x : Integer within e
          var myBranchLocalBindingsTyping = localBindingsTyping;
          registerPattern(branch.pattern(), myBranchLocalBindingsTyping);
          var newExpression = analyzeExpression(branch.expression(), myBranchLocalBindingsTyping);
          return branch.copy(branch.pattern(), newExpression, branch.terminalBranch(), branch.location(), branch.passData(), branch.diagnostics(), branch.id());
        }).toList();
        yield caseExpr.copy(newScrutinee, CollectionConverters$.MODULE$.asScala(newBranches).toSeq(), caseExpr.isNested(), caseExpr.location(), caseExpr.passData(), caseExpr.diagnostics(), caseExpr.id());
      }
      default ->
          ir.mapExpressions((expression) -> analyzeExpression(expression, localBindingsTyping));
    };

    TypeRepresentation inferredType = typePropagation.tryInferringType(mappedIr, localBindingsTyping);
    if (inferredType != null) {
      setInferredType(mappedIr, inferredType);
    }

    if (mappedIr instanceof Expression.Binding b) {
      registerBinding(b, inferredType, localBindingsTyping);
    }

    // The ascriptions are processed later, because we want them to _overwrite_ any type that was inferred.
    processTypeAscription(mappedIr);
    return mappedIr;
  }

  private void processTypeAscription(Expression ir) {
    Optional<TypeSignatures.Signature> ascribedSignature = getOptionalMetadata(ir, TypeSignatures$.MODULE$, TypeSignatures.Signature.class);
    if (ascribedSignature.isPresent()) {
      TypeSignatures.Signature s = ascribedSignature.get();
      TypeRepresentation ascribedType = typeResolver.resolveTypeExpression(s.signature());
      if (ascribedType != null) {
        var previouslyInferredType = getInferredType(ir);
        if (previouslyInferredType != null) {
          if (previouslyInferredType.equals(ascribedType)) {
            logger.debug("redundant type ascription: {} - confirming inferred type {}", ir.showCode(), previouslyInferredType);
          } else {
            logger.debug("type ascription: {} - overwriting inferred type {}", ir.showCode(), previouslyInferredType);
          }

          checkTypeCompatibility(ir, ascribedType, previouslyInferredType);
        }

        setInferredType(ir, ascribedType);
      }
    }
  }

  private void registerBinding(IR binding, TypeRepresentation type, LocalBindingsTyping localBindingsTyping) {
    var metadata = getMetadata(binding, AliasAnalysis$.MODULE$, Info.Occurrence.class);
    var occurrence = metadata.graph().getOccurrence(metadata.id());
    if (occurrence.isEmpty()) {
      logger.debug("registerBinding {}: missing occurrence in graph for {}", binding.showCode(), metadata);
      return;
    }

    if (occurrence.get() instanceof org.enso.compiler.pass.analyse.alias.Graph$Occurrence$Def def) {
      localBindingsTyping.registerBindingType(metadata.graph(), def.id(), type);
    } else {
      throw new CompilerError("Alias analysis occurrence has unexpected type: " + occurrence.get().getClass().getCanonicalName());
    }
  }

  private void registerPattern(Pattern pattern, LocalBindingsTyping localBindingsTyping) {
    switch (pattern) {
      case Pattern.Type typePattern -> {
        var type = typeResolver.resolveTypeExpression(typePattern.tpe());
        registerBinding(typePattern.name(), type, localBindingsTyping);
      }
      case Pattern.Constructor constructorPattern -> {
        for (var innerPattern : CollectionConverters$.MODULE$.asJava(constructorPattern.fields())) {
          registerPattern(innerPattern, localBindingsTyping);
        }
      }
      default -> {
      }
    }
  }

  private void setInferredType(IR ir, TypeRepresentation type) {
    Objects.requireNonNull(type, "type must not be null");
    ir.passData().update(this, new InferredType(type));
  }
}
