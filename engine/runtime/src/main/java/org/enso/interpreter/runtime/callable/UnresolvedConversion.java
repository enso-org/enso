package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.callable.InteropConversionCallNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Simple runtime value representing a yet-unresolved by-name symbol. */
@ExportLibrary(InteropLibrary.class)
public final class UnresolvedConversion implements EnsoObject {
  private final ModuleScope scope;

  /**
   * Creates a new unresolved conversion.
   *
   * @param scope the scope in which this conversion was created
   */
  private UnresolvedConversion(ModuleScope scope) {
    this.scope = scope;
  }

  /** @return the scope this symbol was used in. */
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
   * @return the resolved function definition, or null if not found
   */
  public Function resolveFor(EnsoContext ctx, Type into, Type from) {
    if (from != null) {
      for (var current : from.allTypes(ctx)) {
        Function candidate = scope.lookupConversionDefinition(current, into);
        if (candidate != null) {
          return candidate;
        }
      }
    }
    return scope.lookupConversionDefinition(ctx.getBuiltins().any(), into);
  }

  @Override
  public String toString() {
    return "UnresolvedConversion";
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return this.toString();
  }

  /**
   * Creates an instance of this node.
   *
   * @param scope the scope in which the lookup will occur
   * @return a node representing an unresolved symbol {@code name} in {@code scope}
   */
  public static UnresolvedConversion build(ModuleScope scope) {
    return new UnresolvedConversion(scope);
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

  /** Implements the logic of executing {@link UnresolvedConversion} through the interop library. */
  @ExportMessage
  @ImportStatic(Constants.CacheSizes.class)
  public static class Execute {
    @Specialization
    static Object doDispatch(
        UnresolvedConversion conversion,
        Object[] arguments,
        @Cached InteropConversionCallNode interopConversionCallNode,
        @CachedLibrary("conversion") InteropLibrary thisLib)
        throws ArityException {
      return interopConversionCallNode.execute(
          conversion, EnsoContext.get(thisLib).emptyState(), arguments);
    }
  }
}
