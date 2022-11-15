package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "hash_code",
    description = "Returns has code of the object.")
public abstract class GetHashCodeNode extends Node {
  public static GetHashCodeNode build() {
    return GetHashCodeNodeGen.create();
  }

  public abstract long execute(Object object);

  @Specialization(limit = "3",
      guards = {"object == cachedObject", "interop.hasIdentity(object)"})
  long doExecuteCached(
      Object object,
      @Cached("object") Object cachedObject,
      @CachedLibrary("object") InteropLibrary interop
  ) {
    try {
      return interop.identityHashCode(cachedObject);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(cachedObject, interop);
    }
  }

  @Specialization()
  long doExecuteUncached(
      Object object,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Cached ConditionProfile hasIdentityProfile
  ) {
    if (hasIdentityProfile.profile(interop.hasIdentity(object))) {
      try {
        return interop.identityHashCode(object);
      } catch (UnsupportedMessageException e) {
        throw new PanicException(object, interop);
      }
    } else {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      return object.hashCode();
    }
  }

}
