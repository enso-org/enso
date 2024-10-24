package org.enso.compiler.pass.analyse.types.scope;

import static org.enso.compiler.MetadataInteropHelpers.getMetadata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeInferenceSignatures;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.compiler.pass.analyse.types.TypeResolver;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.MethodDefinitions$;
import org.enso.compiler.pass.resolve.TypeNames$;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.CollectionConverters$;

public class StaticModuleScopeAnalysis implements IRPass {
  public static final StaticModuleScopeAnalysis INSTANCE = new StaticModuleScopeAnalysis();

  private final TypeResolver typeResolver = new TypeResolver();

  private StaticModuleScopeAnalysis() {}

  @Override
  public String toString() {
    return "StaticModuleScopeAnalysis";
  }

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(
            GlobalNames$.MODULE$,
            BindingAnalysis$.MODULE$,
            FullyQualifiedNames$.MODULE$,
            TypeNames$.MODULE$,
            TypeInferenceSignatures.INSTANCE);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    List<IRProcessingPass> passes = List.of();
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    // This has a lot in common with IrToTruffle::processModule - we may want to extract some common
    // parts if it will make sense.
    StaticModuleScope.Builder scopeBuilder = new StaticModuleScope.Builder(moduleContext.getName());
    BindingsMap bindingsMap = getMetadata(ir, BindingAnalysis$.MODULE$, BindingsMap.class);
    processModuleExports(scopeBuilder, ir, bindingsMap);
    processModuleImports(scopeBuilder, ir, bindingsMap);
    processPolyglotImports(scopeBuilder, ir);
    processBindings(scopeBuilder, ir);
    StaticModuleScope scope = scopeBuilder.build();
    ir.passData().update(INSTANCE, scope);
    return ir;
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    // Nothing to do - this pass only works on module-level.
    return ir;
  }

  /**
   * Analogous to {@link
   * org.enso.interpreter.runtime.IrToTruffle#registerModuleImports(BindingsMap)}}
   */
  private void processModuleImports(
      StaticModuleScope.Builder scope, Module module, BindingsMap bindingsMap) {
    bindingsMap
        .resolvedImports()
        .foreach(
            imp -> {
              imp.targets()
                  .foreach(
                      target -> {
                        // System.out.println("Processing import "+imp.importDef().showCode()+" -
                        // target: " + target);
                        if (target instanceof BindingsMap.ResolvedModule resolvedModule) {
                          var importScope =
                              new StaticImportExportScope(resolvedModule.qualifiedName());
                          scope.registerModuleImport(importScope);
                        }
                        // TODO do other kinds of targets need handling? e.g. ResolvedType?
                        return null;
                      });
              return null;
            });
  }

  /**
   * Analogous to {@link
   * org.enso.interpreter.runtime.IrToTruffle#registerModuleExports(BindingsMap)}
   */
  private void processModuleExports(
      StaticModuleScope.Builder scope, Module module, BindingsMap bindingsMap) {
    bindingsMap
        .getDirectlyExportedModules()
        .foreach(
            (exportedMod) -> {
              var exportScope = new StaticImportExportScope(exportedMod.module().qualifiedName());
              scope.registerModuleExport(exportScope);
              return null;
            });
  }

  private void processPolyglotImports(StaticModuleScope.Builder scope, Module module) {
    // TODO [RW]: this is for later iterations, currently resolving polyglot types does not give us
    // much useful info, not better than `Any`
  }

  private void processBindings(StaticModuleScope.Builder scope, Module module) {
    module
        .bindings()
        .foreach(
            binding -> {
              switch (binding) {
                case Definition.Type typ -> processType(scope, typ);
                case Method.Explicit method -> processMethod(scope, method);
                case Method.Conversion conversion -> processConversion(scope, conversion);
                default -> System.out.println(
                    "Unexpected binding type: " + binding.getClass().getCanonicalName());
              }
              return null;
            });
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  private void processType(StaticModuleScope.Builder scope, Definition.Type type) {
    List<AtomType.Constructor> constructors =
        CollectionConverters$.MODULE$.asJava(type.members()).stream()
            .map(
                constructorDef ->
                    new AtomType.Constructor(
                        constructorDef.name().name(), constructorDef.isPrivate()))
            .toList();

    AtomType atomType = new AtomType(type.name().name(), constructors);
    var qualifiedName = scope.getModuleName().createChild(type.name().name());
    var atomTypeScope = TypeScopeReference.atomType(qualifiedName);
    scope.registerType(atomType);
    registerFieldGetters(scope, atomTypeScope, type);
  }

  /**
   * Registers getters for fields of the given type.
   *
   * <p>This should be consistent with logic with AtomConstructor.collectFieldAccessors.
   */
  private void registerFieldGetters(
      StaticModuleScope.Builder scope,
      TypeScopeReference typeScope,
      Definition.Type typeDefinition) {
    HashMap<String, List<TypeRepresentation>> fieldTypes = new HashMap<>();
    for (var constructorDef : CollectionConverters$.MODULE$.asJava(typeDefinition.members())) {
      for (var argumentDef : CollectionConverters$.MODULE$.asJava(constructorDef.arguments())) {
        String fieldName = argumentDef.name().name();
        TypeRepresentation fieldType =
            argumentDef
                .ascribedType()
                .map(typeResolver::resolveTypeExpression)
                .getOrElse(() -> TypeRepresentation.UNKNOWN);
        fieldTypes.computeIfAbsent(fieldName, k -> new ArrayList<>()).add(fieldType);
      }
    }

    for (var entry : fieldTypes.entrySet()) {
      String fieldName = entry.getKey();
      TypeRepresentation mergedType = TypeRepresentation.buildSimplifiedSumType(entry.getValue());
      scope.registerMethod(typeScope, fieldName, mergedType);
    }
  }

  private void processMethod(StaticModuleScope.Builder scope, Method.Explicit method) {
    var typeScope = getTypeAssociatedWithMethod(method, scope.getAssociatedType());
    if (typeScope == null) {
      System.out.println(
          "Failed to process method "
              + method.methodReference().showCode()
              + ", because its type scope could not be resolved.");
      return;
    }
    var typeFromSignature =
        MetadataInteropHelpers.getMetadataOrNull(
            method, TypeInferenceSignatures.INSTANCE, InferredType.class);
    var type = typeFromSignature != null ? typeFromSignature.type() : TypeRepresentation.UNKNOWN;
    var name = method.methodReference().methodName().name();
    scope.registerMethod(typeScope, name, type);
  }

  /**
   * Resolves the type associated with the given method.
   *
   * @param method The method definition to resolve.
   * @param scopeAsscoiatedType The type associated with the scope in which the method is defined.
   *     It will be used as a fallback if the method is deemed to be a module-method.
   * @return the type associated with the method
   */
  TypeScopeReference getTypeAssociatedWithMethod(
      Method.Explicit method, TypeScopeReference scopeAsscoiatedType) {
    // TODO this should be synchronized with declaredConsOpt of IrToTruffle::processModule -
    // probably good to extract a common algorithm
    boolean isStatic = method.isStatic();

    var typePointerOpt = method.methodReference().typePointer();
    if (typePointerOpt.isEmpty()) {
      // A method not associated to a type - this is a module method.
      // TODO should we check isStatic here?
      return scopeAsscoiatedType;
    } else {
      var metadata =
          MetadataInteropHelpers.getMetadataOrNull(
              typePointerOpt.get(), MethodDefinitions$.MODULE$, BindingsMap.Resolution.class);
      if (metadata == null) {
        throw new IllegalStateException(
            "Failed to resolve type pointer for method: " + method.methodReference().showCode());
      }

      return switch (metadata.target()) {
        case BindingsMap.ResolvedType resolvedType -> TypeScopeReference.atomType(
            resolvedType.qualifiedName(), isStatic);
        case BindingsMap.ResolvedModule resolvedModule -> {
          assert !isStatic;
          yield TypeScopeReference.moduleAssociatedType(resolvedModule.qualifiedName());
        }
        default -> throw new IllegalStateException(
            "Unexpected target type: " + metadata.target().getClass().getCanonicalName());
      };
    }
  }

  private void processConversion(StaticModuleScope.Builder scope, Method.Conversion conversion) {
    // TODO later
  }
}
