package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.phase.ImportResolverAlgorithm;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.TopLevelScope;

final class InvokeMethodImportResolver
    extends ImportResolverAlgorithm<EnsoObject, Module, UnresolvedSymbol, Object, Type, Module> {

  private final Module module;
  private final TopLevelScope topScope;

  private InvokeMethodImportResolver(Module module, TopLevelScope topScope) {
    this.module = module;
    this.topScope = topScope;
  }

  @Override
  protected String nameForImport(UnresolvedSymbol name) {
    var fqn = module.getName().pathAsJava();
    String moduleName;
    if (fqn.size() == 2 && "Main".equals(module.getName().item())) {
      moduleName = module.getPackage().libraryName().toString();
    } else {
      moduleName = module.getName().toString();
    }
    var subModuleName = moduleName + "." + name.getName();
    return subModuleName;
  }

  @Override
  protected List<String> partsForImport(UnresolvedSymbol imp) {
    return module.getName().createChild(imp.getName()).pathAsJava();
  }

  @Override
  protected String nameForExport(Object ex) {
    throw new UnsupportedOperationException("nameForExport: " + ex);
  }

  @Override
  protected String nameForType(Type e) {
    return e.getName();
  }

  @Override
  protected List<Object> exportsFor(Module module, String impName) {
    var arr = new ArrayList<Object>();
    arr.addAll(module.getScope().getTypes().keySet());
    return arr;
  }

  @Override
  protected boolean isAll(Object ex) {
    return false;
  }

  @Override
  protected List<String> onlyNames(Object ex) {
    return null;
  }

  @Override
  protected List<String> hiddenNames(Object ex) {
    return null;
  }

  @Override
  protected List<Type> definedEntities(UnresolvedSymbol name) {
    var fqn = nameForImport(name);
    var optionModule = topScope.getModule(fqn);
    return optionModule.map(m -> m.getScope().getAssociatedType()).stream().toList();
  }

  @Override
  protected Module loadLibraryModule(LibraryName libraryName, String moduleName)
      throws IOException {
    var optionModule = topScope.getModule(moduleName);
    return optionModule.isPresent() ? optionModule.get() : null;
  }

  @Override
  protected EnsoObject createResolvedImport(UnresolvedSymbol imp, List<Object> exp, Module m) {
    return m.getScope().getAssociatedType();
  }

  @Override
  protected EnsoObject createResolvedType(UnresolvedSymbol imp, List<Object> exp, Type typ) {
    return typ;
  }

  @Override
  protected EnsoObject createErrorPackageCoundNotBeLoaded(
      UnresolvedSymbol imp, String impName, String loadingError) {
    return null;
  }

  @Override
  protected EnsoObject createErrorModuleDoesNotExist(UnresolvedSymbol imp, String impName) {
    return null;
  }

  @CompilerDirectives.TruffleBoundary
  private static boolean isNamedAsAssociatedType(Type t) {
    var at = t.getDefinitionScope().getAssociatedType();
    var byType = at == t;
    if (byType) {
      return true;
    }
    var byName = at.getName().equals(t.getName());
    if (!byName) {
      return false;
    }
    var atq = at.getQualifiedName();
    var tq = t.getQualifiedName();
    var tqParentOption = tq.getParent();
    return tqParentOption.isDefined() && atq.equals(tqParentOption.get());
  }

  @CompilerDirectives.TruffleBoundary
  static Object tryResolve(Type t, UnresolvedSymbol symbol, EnsoContext ctx) {
    if (!isNamedAsAssociatedType(t)) {
      return null;
    }
    var scope = t.getDefinitionScope();
    var module = scope.getModule();
    var resolver = new InvokeMethodImportResolver(module, ctx.getTopScope());
    var found = resolver.tryResolveImport(module, symbol);
    System.out.println("Found " + found);
    // if we represent a Module
    var optionType = scope.getType(symbol.getName());
    if (optionType.isPresent()) {
      // assert found != null;
      return optionType.get();
    }
    var moduleName =
        module.getName().item().equals("Main")
            ? module.getPackage().libraryName().toString()
            : module.getName().toString();
    var subModuleName = moduleName + "." + symbol.getName();
    var optionModule = ctx.getTopScope().getModule(subModuleName);
    if (optionModule.isPresent()) {
      var subType = optionModule.get().getScope().getAssociatedType();
      if (subType != null) {
        assert found == subType : "Found " + found + " subtype: " + subType;
        return subType;
      }
    }
    assert found == null;
    return null;
  }
}
