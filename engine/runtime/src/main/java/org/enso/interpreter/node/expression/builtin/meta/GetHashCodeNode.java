package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ValueProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Meta", name = "hash_code", description = "Returns has code of the object.")
public abstract class GetHashCodeNode extends Node {
  public static GetHashCodeNode build() {
    return GetHashCodeNodeGen.create();
  }

  public abstract long execute(Object object);

  @Specialization(guards = "interop.hasIdentity(object)")
  long doExecuteCached(
      Object object,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Cached("createClassProfile()") ValueProfile objectClassProfile) {
    try {
      return interop.identityHashCode(objectClassProfile.profile(object));
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      throw new PanicException(object, interop);
    }
  }

  @TruffleBoundary
  @Specialization(guards = "!interop.hasIdentity(object)")
  long doExecuteUncached(Object object, @CachedLibrary(limit = "5") InteropLibrary interop) {
    return object.hashCode();
  }
}
