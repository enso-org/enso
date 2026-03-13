package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import org.enso.compiler.phase.ImportResolverAlgorithm;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

final class InvokeMethodImportResolver
    extends ImportResolverAlgorithm<
        EnsoObject,
        Module,
        UnresolvedSymbol,
        Object,
        Type,
        Module,
        AtomConstructor,
        Function,
        Function,
        Function> {

  private final Module module;
  private final EnsoContext ctx;

  private InvokeMethodImportResolver(Module module, EnsoContext ctx) {
    this.module = module;
    this.ctx = ctx;
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
    return module.getName().createChild(imp.getName()).createChild("any").pathAsJava();
  }

  @Override
  protected String nameForExport(Object ex) {
    throw new AssertionError("not used: " + ex);
  }

  @Override
  protected String nameForType(Type e) {
    return e.getName();
  }

  @Override
  protected String nameForConstructor(AtomConstructor cons) {
    return cons.getName();
  }

  @Override
  protected String nameForModuleMethod(Function function) {
    return function.getName();
  }

  @Override
  protected String nameForExtensionMethod(Function function) {
    return function.getName();
  }

  @Override
  protected String nameForConversionMethod(Function function) {
    return function.getName();
  }

  @Override
  protected List<Object> exportsFor(Module module, String impName) {
    return Collections.emptyList();
  }

  @Override
  protected List<String> onlyNames(Object ex) {
    return null;
  }

  @Override
  protected List<Type> definedEntities(java.util.List<String> parts, UnresolvedSymbol symbol) {
    return module.getScope().getAllTypes(symbol.getName());
  }

  @Override
  protected List<AtomConstructor> definedConstructors(
      java.util.List<String> parts, UnresolvedSymbol symbol) {
    return Collections.emptyList();
  }

  @Override
  protected List<Function> definedModuleMethods(
      java.util.List<String> parts, UnresolvedSymbol symbol) {
    return Collections.emptyList();
  }

  @Override
  protected List<Function> definedExtensionMethods(
      java.util.List<String> parts, UnresolvedSymbol imp) {
    return null;
  }

  @Override
  protected List<Function> definedConversionMethods(
      java.util.List<String> parts, UnresolvedSymbol imp) {
    return null;
  }

  @Override
  protected Module loadLibraryModule(LibraryName libraryName, String moduleName)
      throws IOException {
    var optionModule = ctx.getTopScope().getModule(moduleName);
    return optionModule.orElse(null);
  }

  @Override
  protected EnsoObject createResolvedImport(UnresolvedSymbol imp, List<Object> exp, Module m) {
    var scope = m.compileScope(ctx);
    return scope.getAssociatedType();
  }

  @Override
  protected EnsoObject createResolvedType(UnresolvedSymbol imp, List<Object> exp, Type typ) {
    return typ;
  }

  @Override
  protected EnsoObject createResolvedConstructor(
      UnresolvedSymbol imp, List<Object> exp, AtomConstructor cons) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  protected EnsoObject createResolvedModuleMethod(
      UnresolvedSymbol imp, List<Object> exp, Function function) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  protected EnsoObject createResolvedExtensionMethods(
      UnresolvedSymbol imp, List<Object> exp, List<Function> functions) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  protected EnsoObject createResolvedConversionMethods(
      UnresolvedSymbol imp, List<Object> exp, List<Function> functions) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  protected EnsoObject createErrorPackageCouldNotBeLoaded(
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
    var resolver = new InvokeMethodImportResolver(module, ctx);
    var found = resolver.tryResolveImport(module, symbol);
    return found;
  }
}
