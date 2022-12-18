package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.expression.builtin.meta.IsSameObjectNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.type.TypesGen;

/** An implementation of the payload check against the expected panic type. */
@NodeInfo(shortName = "IsPayloadOfPanicTypeNode")
public abstract class IsPayloadOfPanicTypeNode extends Node {
  private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
  private @Child TypeOfNode typeOfNode = TypeOfNode.build();
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  public static IsPayloadOfPanicTypeNode build() {
    return IsPayloadOfPanicTypeNodeGen.create();
  }

  public abstract boolean execute(Object panicType, Object payload);

  @Specialization
  boolean doAtom(AtomConstructor consr, Object payload) {
    Object panicType = consr.getType();
    return execute(panicType, payload);
  }

  @Specialization(guards = {"interop.isMetaObject(panicType)"})
  boolean doPolyglotType(
      Object panicType, Object payload, @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      return interop.isMetaInstance(panicType, payload);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = "isAnyType(panicType)")
  boolean doAnyType(Object panicType, Object payload) {
    return true;
  }

  @Specialization(
      guards = {
        "isArrayType(panicType)",
        "interop.hasArrayElements(payload)",
        "!types.hasType(payload)",
        "interop.hasMetaObject(payload)"
      })
  public boolean doPolyglotArray(
      Object panicType,
      Object payload,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @CachedLibrary(limit = "3") TypesLibrary types) {
    return true;
  }

  boolean isAnyType(Object panicType) {
    return EnsoContext.get(this).getBuiltins().any() == panicType;
  }

  boolean isArrayType(Object panicType) {
    return EnsoContext.get(this).getBuiltins().array() == panicType;
  }

  @Fallback
  boolean doType(Object panicType, Object payload) {
    Object tpeOfPayload = typeOfNode.execute(payload);
    if (profile.profile(isSameObject.execute(panicType, tpeOfPayload))) {
      return true;
    } else if (TypesGen.isType(tpeOfPayload)) {
      Type tpe = TypesGen.asType(tpeOfPayload);
      Type superTpe = tpe.getSupertype();

      while (tpe != superTpe && superTpe != null) {
        boolean testSuperTpe = isSameObject.execute(panicType, superTpe);
        if (testSuperTpe) {
          return true;
        }
        tpe = superTpe;
        superTpe = superTpe.getSupertype();
      }
    }
    return false;
  }
}
