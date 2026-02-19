package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.InteropMethodCallNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.Package;

/** Simple runtime value representing a yet-unresolved by-name symbol. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class UnresolvedSymbol extends EnsoObject {
  private final String name;
  private final ModuleScope scope;

  /**
   * Creates a new unresolved symbol.
   *
   * @param name the name of this symbol
   * @param scope the scope in which this symbol was created
   */
  private UnresolvedSymbol(String name, ModuleScope scope) {
    this.name = name;
    this.scope = scope;
  }

  /**
   * Gets the symbol name.
   *
   * @return the name of this symbol
   */
  public String getName() {
    return name;
  }

  /**
   * @return the scope this symbol was used in.
   */
  public ModuleScope getScope() {
    return scope;
  }

  /**
   * Resolves the symbol for a given hierarchy of constructors.
   *
   * <p>The constructors are checked in the first to last order, and the first match for this symbol
   * is returned. This is useful for certain subtyping relations, such as "any constructor is a
   * subtype of Any" or "Nat is a subtype of Int, is a subtype of Number".
   *
   * @param node the node that performs the query
   * @param type the type for which this symbol should be resolved
   * @return the resolved function definition and type it was resolved in, or null if not found
   */
  @TruffleBoundary
  public FunctionAndType resolveFor(Node node, Type type) {
    if (type != null) {
      for (var current : type.allTypes(EnsoContext.get(node))) {
        Function candidate = scope.lookupMethodDefinition(current, name);
        if (candidate != null) {
          ensureIsAccessible(node, candidate);
          return new FunctionAndType(candidate, current);
        }
      }
    }
    return null;
  }

  private void ensureIsAccessible(Node node, Function function) throws PanicException {
    var isPrivateCheckDisabled = EnsoContext.get(node).isPrivateCheckDisabled();
    CompilerAsserts.compilationConstant(isPrivateCheckDisabled);
    if (!isPrivateCheckDisabled && function.getSchema().isProjectPrivate()) {
      var thisPkg = getThisProject();
      var targetPkg = getFunctionProject(function);
      if (thisPkg != targetPkg) {
        throw makePrivateAccessPanic(node, function);
      }
    }
  }

  private PanicException makePrivateAccessPanic(Node node, Function targetFunction) {
    String thisProjName = getThisProject().libraryName().qualifiedName();
    String targetProjName = null;
    if (getFunctionProject(targetFunction) instanceof Package<TruffleFile> targetProj) {
      targetProjName = targetProj.libraryName().qualifiedName();
    }
    var funcName = targetFunction.getName();
    var err =
        EnsoContext.get(node)
            .getBuiltins()
            .error()
            .makePrivateAccessError(thisProjName, targetProjName, funcName, null);
    return new PanicException(err, node);
  }

  private Package<TruffleFile> getThisProject() {
    return scope.getModule().getPackage();
  }

  private static Package<TruffleFile> getFunctionProject(Function function) {
    var cons = AtomConstructor.accessorFor(function);
    if (cons != null) {
      return cons.getDefinitionScope().getModule().getPackage();
    }
    cons = MethodRootNode.constructorFor(function);
    if (cons != null) {
      return cons.getDefinitionScope().getModule().getPackage();
    }
    if (function.getCallTarget().getRootNode() instanceof EnsoRootNode ensoRootNode) {
      return ensoRootNode.getModuleScope().getModule().getPackage();
    }
    return null;
  }

  @Override
  public String toString() {
    return "UnresolvedSymbol<" + this.name + ">";
  }

  @ExportMessage
  @TruffleBoundary
  @Override
  public String toDisplayString(boolean allowSideEffects) {
    return this.toString();
  }

  /**
   * Creates an instance of this node.
   *
   * @param name the name that is unresolved
   * @param scope the scope in which the lookup will occur
   * @return a node representing an unresolved symbol {@code name} in {@code scope}
   */
  public static UnresolvedSymbol build(String name, ModuleScope scope) {
    return new UnresolvedSymbol(name, scope);
  }

  /**
   * Marks this object as executable through the interop library.
   *
   * @return always true
   */
  @ExportMessage
  public boolean isExecutable() {
    return true;
  }

  /** Implements the logic of executing {@link UnresolvedSymbol} through the interop library. */
  @ExportMessage
  @ImportStatic(Constants.CacheSizes.class)
  public static class Execute {
    @Specialization
    static Object doDispatch(
        UnresolvedSymbol symbol,
        Object[] arguments,
        @Cached InteropMethodCallNode interopMethodCallNode,
        @CachedLibrary("symbol") InteropLibrary thisLib)
        throws ArityException {
      return interopMethodCallNode.execute(
          symbol, EnsoContext.get(thisLib).currentState(), arguments);
    }
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@Bind Node node) {
    var ctx = EnsoContext.get(node);
    return ctx.getBuiltins().function();
  }
}
