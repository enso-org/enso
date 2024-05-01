package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
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
import com.oracle.truffle.api.nodes.NodeUtil;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.callable.InteropMethodCallNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.graalvm.collections.Pair;

/** Simple runtime value representing a yet-unresolved by-name symbol. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class UnresolvedSymbol implements EnsoObject {
  public static boolean logEnabled = false;
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

  private boolean isInProblematicState(Node node, Type type) {
    return node.getRootNode() != null
        && node.getRootNode().getName() != null
        && node.getRootNode().getName().equals("Error.is_a<arg-0>")
        && name.equals("==")
        && type.getQualifiedName().toString().equals("Standard.Base.Error.Error.type")
        && logEnabled;
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
  public Pair<Function, Type> resolveFor(Node node, Type type) {
    if (type != null) {
      var nodeName = node.getRootNode() == null ? "null" : node.getRootNode().getName();
      log(
          "Resolving for: symbol=%s, node=%s, type=%s"
              .formatted(name, nodeName, type.getQualifiedName().toString()));
      for (var current : type.allTypes(EnsoContext.get(node))) {
        log("Current type = " + current.getQualifiedName().toString());
        Function candidate = scope.lookupMethodDefinition(current, name);
        if (candidate != null) {
          log("Resolved to " + candidate);
          return Pair.create(candidate, current);
        }
      }
    }
    log("Unresolved (returning null)");
    if (isInProblematicState(node, type)) {
      log("Problematic state detected");
      var allTypes = type.allTypes(EnsoContext.get(node));
      var allTypeNames = Arrays
          .stream(allTypes)
          .map(tp -> tp.getQualifiedName().toString())
          .collect(Collectors.toUnmodifiableList());
      log("All type names = " + allTypeNames);
      log("=== Truffle AST ===");
      // Dump the entire Truffle AST
      NodeUtil.printTree(System.out, node);
    }
    return null;
  }

  @TruffleBoundary
  private static void log(String message) {
    if (logEnabled) {
      System.out.println("[UnresolvedSymbol]: " + message);
    }
  }

  @Override
  public String toString() {
    return "UnresolvedSymbol<" + this.name + ">";
  }

  @ExportMessage
  @TruffleBoundary
  String toDisplayString(boolean allowSideEffects) {
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
          symbol, EnsoContext.get(thisLib).emptyState(), arguments);
    }
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@Bind("$node") Node node) {
    var ctx = EnsoContext.get(node);
    return ctx.getBuiltins().function();
  }
}
