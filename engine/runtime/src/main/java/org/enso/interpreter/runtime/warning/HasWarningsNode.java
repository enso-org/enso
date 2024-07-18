package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** A node that checks if a value has any warnings attached. */
@GenerateUncached
public abstract class HasWarningsNode extends Node {

  @NeverDefault
  public static HasWarningsNode create() {
    return HasWarningsNodeGen.create();
  }

  public static HasWarningsNode getUncached() {
    return HasWarningsNodeGen.getUncached();
  }

  public abstract boolean execute(Object value);

  @Specialization
  boolean doWithWarn(WithWarnings withWarn) {
    return true;
  }

  // TODO: Create a ArrayLikeHasWarningsNode that caches the hasWarning field inside
  //  Array and Vector objects.
  /** A vector has a warning if one of its elements has warning. */
  @Specialization(guards = "isEnsoVector(vector, typesLib)")
  boolean doVector(
      Object vector,
      @CachedLibrary(limit = "3") TypesLibrary typesLib,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode,
      @Cached HasWarningsNode elemHasWarnsNode) {
    assert InteropLibrary.getUncached().hasArrayElements(vector);
    long len = lengthNode.executeLength(vector);
    for (long i = 0; i < len; i++) {
      Object elem;
      try {
        elem = atNode.executeAt(vector, i);
      } catch (InvalidArrayIndexException e) {
        throw CompilerDirectives.shouldNotReachHere(e);
      }
      var elemHasWarn = elemHasWarnsNode.execute(elem);
      if (elemHasWarn) {
        return true;
      }
    }
    return false;
  }

  @Fallback
  boolean doOther(Object value) {
    return false;
  }

  protected boolean isEnsoVector(Object obj, TypesLibrary typesLib) {
    if (typesLib.hasType(obj)) {
      var ctx = EnsoContext.get(this);
      var vecType = ctx.getBuiltins().vector();
      var arrayType = ctx.getBuiltins().array();
      var tp = typesLib.getType(obj);
      return tp == vecType || tp == arrayType;
    }
    return false;
  }
}
