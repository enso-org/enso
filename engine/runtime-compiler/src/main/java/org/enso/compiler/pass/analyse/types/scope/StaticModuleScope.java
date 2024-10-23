package org.enso.compiler.pass.analyse.types.scope;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.pkg.QualifiedName;
import scala.Option;

/**
 * This is a sibling to the ModuleScope.
 *
 * <p>The ModuleScope is the runtime representation of a module, optimized for fast runtime
 * dispatch. The StaticModuleScope is an analogous structure, that can be used by static analysis
 * passes at compilation time.
 *
 * <p>It is also similar to the BindingsMap structure. In fact, it may be possible to merge the two
 * modules in the future, as StaticModuleScope is a more general variant. The BindingsMap only deals
 * with Types and their Constructors that are used during static resolution of some names. This
 * class also keeps track of all defined methods, to facilitate type checking. I'm keeping these
 * separate for now as it is easier to create a prototype that way. If later we find out they have
 * enough of similarity, we should merge them.
 */
public final class StaticModuleScope implements ProcessingPass.Metadata {
  private final QualifiedName moduleName;
  private final TypeScopeReference associatedType;
  private final List<StaticImportExportScope> imports = new ArrayList<>();
  private final List<StaticImportExportScope> exports = new ArrayList<>();

  StaticModuleScope(QualifiedName moduleName) {
    this.moduleName = moduleName;
    this.associatedType = TypeScopeReference.moduleAssociatedType(moduleName);
  }

  void registerType(AtomType type) {
    var previous = typesDefinedHere.putIfAbsent(type.getName(), type);
    if (previous != null) {
      throw new IllegalStateException("Type already defined: " + type.getName());
    }
  }

  void registerMethod(TypeScopeReference parentType, String name, TypeRepresentation type) {
    var typeMethods = methods.computeIfAbsent(parentType, k -> new HashMap<>());
    typeMethods.put(name, type);
  }

  public TypeScopeReference getAssociatedType() {
    return associatedType;
  }

  private final Map<String, AtomType> typesDefinedHere = new HashMap<>();
  private final Map<TypeScopeReference, Map<String, TypeRepresentation>> methods = new HashMap<>();

  public static StaticModuleScope forIR(Module module) {
    return MetadataInteropHelpers.getMetadata(
        module, StaticModuleScopeAnalysis.INSTANCE, StaticModuleScope.class);
  }

  /** Aligned with @link{ModuleScope#getMethodForType} */
  public TypeRepresentation getMethodForType(TypeScopeReference type, String name) {
    var typeMethods = methods.get(type);
    if (typeMethods == null) {
      return null;
    }

    return typeMethods.get(name);
  }

  /** Aligned with @link{ModuleScope#getExportedMethod} */
  public TypeRepresentation getExportedMethod(
      TypeScopeReference type, String name, ModuleResolver moduleResolver) {
    var here = getMethodForType(type, name);
    if (here != null) {
      return here;
    }

    return exports.stream()
        .map(scope -> scope.materialize(moduleResolver).getMethodForType(type, name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  @Override
  public String metadataName() {
    return "StaticModuleScope";
  }

  @Override
  public ProcessingPass.Metadata prepareForSerialization(CompilerStub compiler) {
    return this;
  }

  @Override
  public Option<ProcessingPass.Metadata> restoreFromSerialization(CompilerStub compiler) {
    return Option.apply(this);
  }

  @Override
  public Option<ProcessingPass.Metadata> duplicate() {
    return Option.empty();
  }

  public QualifiedName getModuleName() {
    return moduleName;
  }

  public void registerModuleImport(StaticImportExportScope importScope) {
    imports.add(importScope);
  }

  public List<StaticImportExportScope> getImports() {
    return imports;
  }

  public void registerModuleExport(StaticImportExportScope exportScope) {
    exports.add(exportScope);
  }

  public List<StaticImportExportScope> getExports() {
    return exports;
  }
}
