package org.enso.compiler.pass.analyse.types.scope;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
  private final List<StaticImportExportScope> imports;
  private final List<StaticImportExportScope> exports;
  private final Map<String, AtomTypeDefinition> typesDefinedHere;
  private final Map<TypeScopeReference, Map<String, TypeRepresentation>> methods;

  // The Map maps target types to a set of source types that can be converted to it.
  // TODO conversions can also have optional arguments, we should include this (so no longer a Set
  // but Map)
  private final Map<TypeScopeReference, Set<TypeScopeReference>> conversions;

  private StaticModuleScope(
      QualifiedName moduleName,
      TypeScopeReference associatedType,
      List<StaticImportExportScope> imports,
      List<StaticImportExportScope> exports,
      Map<String, AtomTypeDefinition> typesDefinedHere,
      Map<TypeScopeReference, Map<String, TypeRepresentation>> methods,
      Map<TypeScopeReference, Set<TypeScopeReference>> conversions) {
    this.moduleName = moduleName;
    this.associatedType = associatedType;
    this.imports = imports;
    this.exports = exports;
    this.typesDefinedHere = typesDefinedHere;
    this.methods = methods;
    this.conversions = conversions;
  }

  static final class Builder {
    private final QualifiedName moduleName;
    private final TypeScopeReference associatedType;
    private final List<StaticImportExportScope> imports = new ArrayList<>();
    private final List<StaticImportExportScope> exports = new ArrayList<>();
    private final Map<String, AtomTypeDefinition> typesDefinedHere = new HashMap<>();
    private final Map<TypeScopeReference, Map<String, TypeRepresentation>> methods =
        new HashMap<>();
    private final Map<TypeScopeReference, Set<TypeScopeReference>> conversions = new HashMap<>();

    private boolean sealed = false;

    private void checkSealed() {
      if (sealed) {
        throw new IllegalStateException(
            "`build` method has already been called, this builder should no longer be modified.");
      }
    }

    Builder(QualifiedName moduleName) {
      this.moduleName = moduleName;
      this.associatedType = TypeScopeReference.moduleAssociatedType(moduleName);
    }

    public StaticModuleScope build() {
      sealed = true;
      return new StaticModuleScope(
          moduleName,
          associatedType,
          Collections.unmodifiableList(imports),
          Collections.unmodifiableList(exports),
          Collections.unmodifiableMap(typesDefinedHere),
          unmodifiableNestedMap(methods),
          unmodifiableNestedSet(conversions));
    }

    private <A, B, C> Map<A, Map<B, C>> unmodifiableNestedMap(Map<A, Map<B, C>> m) {
      var result = new HashMap<A, Map<B, C>>();
      for (var entry : m.entrySet()) {
        result.put(entry.getKey(), Collections.unmodifiableMap(entry.getValue()));
      }
      return Collections.unmodifiableMap(result);
    }

    private <A, B> Map<A, Set<B>> unmodifiableNestedSet(Map<A, Set<B>> m) {
      var result = new HashMap<A, Set<B>>();
      for (var entry : m.entrySet()) {
        result.put(entry.getKey(), Collections.unmodifiableSet(entry.getValue()));
      }
      return Collections.unmodifiableMap(result);
    }

    QualifiedName getModuleName() {
      return moduleName;
    }

    public TypeScopeReference getAssociatedType() {
      return associatedType;
    }

    void registerType(AtomTypeDefinition type) {
      checkSealed();
      var previous = typesDefinedHere.putIfAbsent(type.getName(), type);
      if (previous != null) {
        throw new IllegalStateException("Type already defined: " + type.getName());
      }
    }

    void registerMethod(TypeScopeReference parentType, String name, TypeRepresentation type) {
      checkSealed();
      var typeMethods = methods.computeIfAbsent(parentType, k -> new HashMap<>());
      typeMethods.put(name, type);
    }

    void registerConversionMethod(TypeScopeReference toType, TypeScopeReference fromType) {
      assert toType.getKind() == TypeScopeReference.Kind.ATOM_TYPE;
      assert fromType.getKind() == TypeScopeReference.Kind.ATOM_TYPE;
      Set<TypeScopeReference> sourcesSet =
          conversions.computeIfAbsent(toType, k -> new HashSet<>());
      boolean isNew = sourcesSet.add(fromType);
      if (!isNew) {
        throw new IllegalStateException(
            "Conversion already defined: " + fromType + " -> " + toType);
      }
    }

    public void addImport(StaticImportExportScope importScope) {
      checkSealed();
      imports.add(importScope);
    }

    public void addExport(StaticImportExportScope exportScope) {
      checkSealed();
      exports.add(exportScope);
    }
  }

  public TypeScopeReference getAssociatedType() {
    return associatedType;
  }

  public static StaticModuleScope forIR(Module module) {
    return MetadataInteropHelpers.getMetadata(
        module, StaticModuleScopeAnalysis.INSTANCE, StaticModuleScope.class);
  }

  public TypeRepresentation getMethodForType(TypeScopeReference type, String name) {
    var typeMethods = methods.get(type);
    if (typeMethods == null) {
      return null;
    }

    return typeMethods.get(name);
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

  public List<StaticImportExportScope> getImports() {
    return imports;
  }

  public List<StaticImportExportScope> getExports() {
    return exports;
  }

  public TypeRepresentation getConversionFor(TypeScopeReference target, TypeScopeReference source) {
    var conversionsOnType = conversions.get(target);
    if (conversionsOnType == null) {
      return null;
    }

    boolean conversionExists = conversionsOnType.contains(source);
    if (!conversionExists) {
      return null;
    }

    // TODO conversions can contain optional arguments
    // so we cannot really return a function type until we are capable of returning types with
    // optional arguments
    // then we'll need to change the conversions data structure to store these arguments too
    // For now let's just return an unknown non-null type.
    return TypeRepresentation.UNKNOWN;
  }

  public AtomTypeDefinition getType(String name) {
    return typesDefinedHere.get(name);
  }

  @Override
  public String toString() {
    return "StaticModuleScope{" + moduleName + "}";
  }
}
