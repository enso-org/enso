package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeInferenceSignatures;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.MethodDefinitions$;
import org.enso.compiler.pass.resolve.TypeNames$;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.CollectionConverters$;

import java.util.List;
import java.util.UUID;

public class StaticModuleScopeAnalysis implements IRPass {
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
  public String toString() {
    return "StaticModuleScopeAnalysis";
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    List<IRPass> passes =
        List.of(
            GlobalNames$.MODULE$,
            FullyQualifiedNames$.MODULE$,
            TypeNames$.MODULE$,
            TypeInferenceSignatures.INSTANCE
        );
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRPass> invalidatedPasses() {
    List<IRPass> passes =
        List.of(
            GlobalNames$.MODULE$,
            FullyQualifiedNames$.MODULE$,
            TypeNames$.MODULE$,
            TypeInferenceSignatures.INSTANCE
        );
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    // This has a lot in common with IrToTruffle::processModule - we may want to extract some common parts if it will make sense.
    StaticModuleScope scope = new StaticModuleScope(moduleContext.getName());
    ir.bindings().foreach(binding -> {
      switch (binding) {
        case Definition.Type typ -> processType(scope, typ);
        case Method.Explicit method -> processMethod(scope, method);
        case Method.Conversion conversion -> processConversion(scope, conversion);
        default -> System.out.println("Unexpected binding type: " + binding.getClass().getCanonicalName());
      }
      return null;
    });

    // TODO process imports/exports

    return ir;
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    // Nothing to do - this pass only works on module-level.
    return ir;
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  private void processType(StaticModuleScope scope, Definition.Type type) {
    List<AtomType.Constructor> constructors =
        CollectionConverters$.MODULE$.asJava(type.members())
            .stream()
            .map(constructorDef ->
                new AtomType.Constructor(
                    constructorDef.name().name(),
                    constructorDef.isPrivate()
                )
            )
            .toList();

    AtomType atomType = new AtomType(type.name().name(), constructors, scope);
    // TODO register field getters - ideally common logic with AtomConstructor.collectFieldAccessors
    scope.registerType(atomType);
  }

  private void processMethod(StaticModuleScope scope, Method.Explicit method) {
    var typeScope = getTypeAssociatedWithMethod(scope, method);
    var typeFromSignature = MetadataInteropHelpers.getMetadataOrNull(method, TypeInferenceSignatures.INSTANCE, InferredType.class);
    var type = typeFromSignature != null ? typeFromSignature.type() : TypeRepresentation.UNKNOWN;
    var name = method.methodReference().methodName().name();
    scope.registerMethod(typeScope, name, type);
  }

  StaticModuleScope.TypeScopeReference getTypeAssociatedWithMethod(StaticModuleScope scope, Method.Explicit method) {
    // TODO this should be synchronized with declaredConsOpt of IrToTruffle::processModule - probably good to extract a common algorithm
    boolean isStatic = method.isStatic();

    var typePointerOpt = method.methodReference().typePointer();
    if (typePointerOpt.isEmpty()) {
      assert !isStatic;
      // A method not associated to a type - this is a module method.
      return scope.getAssociatedType();
    } else {
      var metadata = MetadataInteropHelpers.getMetadataOrNull(typePointerOpt.get(), MethodDefinitions$.MODULE$, BindingsMap.Resolution.class);
      if (metadata == null) {
        throw new IllegalStateException("Method type pointer does not have MethodDefinition metadata. Should this be compiler error?");
      }

      return switch (metadata.target()) {
        case BindingsMap.ResolvedType resolvedType ->
          // TODO more encapsulated handling of the static scope
            isStatic ?
                new StaticModuleScope.TypeScopeReference(resolvedType.qualifiedName().createChild("type")) :
                new StaticModuleScope.TypeScopeReference(resolvedType.qualifiedName());
        case BindingsMap.ResolvedModule resolvedModule -> {
          assert !isStatic;
          yield new StaticModuleScope.TypeScopeReference(resolvedModule.qualifiedName());
        }
        default ->
            throw new IllegalStateException("Unexpected target type: " + metadata.target().getClass().getCanonicalName());
      };
    }
  }

  private void processConversion(StaticModuleScope scope, Method.Conversion conversion) {
    // TODO later
  }
}
