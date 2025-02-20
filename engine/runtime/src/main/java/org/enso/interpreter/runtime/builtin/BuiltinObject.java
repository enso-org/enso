package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * Base class for every Enso builtin object. Not type. Note that base class for a builtin type is
 * {@link Builtin}.
 *
 * <p>In other words, this class represents an object of builtin type in a similar way that {@link
 * org.enso.interpreter.runtime.data.atom.Atom} represents an object of a non-builtin type.
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public abstract class BuiltinObject extends EnsoObject {

  @ExportMessage
  public final boolean hasType() {
    return true;
  }

  /**
   * Returns the name of the builtin as saved inside {@link Builtins#builtinsByName}. Not fully
   * qualified.
   *
   * @return
   */
  protected abstract String builtinName();

  protected final Type getBuiltinType(Node node) {
    return GetType.uncached(this, node);
  }

  /**
   * Must return false, otherwise if a builtin object is passed to a host method that has a single
   * {@code Object} argument, host interop would convert the builtin object to a {@code Map} with
   * all its members. Even if the builtin object is, e.g., a number of a date.
   *
   * <p>Must return false as long as all our stdlib Java methods accept {@code Object} and not
   * {@link org.graalvm.polyglot.Value} as arguments comming from Enso.
   */
  @ExportMessage
  public final boolean hasMembers() {
    return false;
  }

  @ExportMessage
  public final Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  public final boolean hasMetaObject() {
    return true;
  }

  @ExportMessage(name = "getType", library = TypesLibrary.class)
  @ExportMessage(name = "getMetaObject", library = InteropLibrary.class)
  public static final class GetType {

    GetType() {}

    /**
     * Caching on class of the receiver - as long as there is the same class, its {@link
     * #builtinName()} method will return the same value. Note that we don't want to cache on the
     * builtin name, as that would create a separate polymorph cache for every instance of the
     * receiver.
     */
    @Specialization(
        guards = {"cachedReceiverClass == receiver.getClass()", "getCtx(node) == cachedCtx"},
        limit = "1")
    public static Type doItCached(
        BuiltinObject receiver,
        @Bind("$node") Node node,
        @Cached("receiver.getClass()") Class<? extends BuiltinObject> cachedReceiverClass,
        @Cached(value = "getCtx(node)", allowUncached = true) EnsoContext cachedCtx,
        @Cached(value = "getBuiltinType(receiver, cachedCtx)", allowUncached = true)
            Builtin cachedBuiltinType) {
      return cachedBuiltinType.getType();
    }

    @Specialization(replaces = "doItCached")
    public static Type uncached(BuiltinObject receiver, @Bind("$node") Node node) {
      var ctx = getCtx(node);
      return getBuiltinType(receiver, ctx).getType();
    }

    @TruffleBoundary
    public static Builtin getBuiltinType(BuiltinObject receiver, EnsoContext ctx) {
      return ctx.getBuiltins().getBuiltinType(receiver.builtinName());
    }

    @Idempotent
    public static EnsoContext getCtx(Node node) {
      return EnsoContext.get(node);
    }
  }
}
