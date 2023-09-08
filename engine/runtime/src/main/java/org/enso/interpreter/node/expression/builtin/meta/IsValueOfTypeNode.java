package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.type.TypesGen;

/** An implementation of the payload check against the expected panic type. */
@NodeInfo(shortName = "IsValueOfTypeNode")
public abstract class IsValueOfTypeNode extends Node {
  public static IsValueOfTypeNode build() {
    return IsValueOfTypeNodeGen.create();
  }

  public abstract boolean execute(Object expectedType, Object payload);

  @Specialization(guards = {"types.hasType(payload)"})
  boolean doTyped(
      Object expectedType,
      Object payload,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached Typed typed) {
    return typed.execute(expectedType, payload);
  }

  @Specialization(guards = {"!types.hasType(payload)"})
  boolean doPolyglot(
      Object expectedType,
      Object payload,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached Untyped untyped) {
    return untyped.execute(expectedType, payload);
  }

  private static boolean typeAndCheck(
      Object payload,
      Object expectedType,
      TypeOfNode typeOfNode,
      IsSameObjectNode isSameObject,
      CountingConditionProfile isSameObjectProfile) {
    Object tpeOfPayload = typeOfNode.execute(payload);
    if (isSameObjectProfile.profile(isSameObject.execute(expectedType, tpeOfPayload))) {
      return true;
    } else if (TypesGen.isType(tpeOfPayload)) {
      Type tpe = TypesGen.asType(tpeOfPayload);
      var ctx = EnsoContext.get(typeOfNode);
      for (var superTpe : tpe.allTypes(ctx)) {
        boolean testSuperTpe = isSameObject.execute(expectedType, superTpe);
        if (testSuperTpe) {
          return true;
        }
      }
    }
    return false;
  }

  abstract static class Typed extends Node {
    private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
    private @Child TypeOfNode typeOfNode = TypeOfNode.build();
    private final CountingConditionProfile profile = CountingConditionProfile.create();

    abstract boolean execute(Object expectedType, Object payload);

    @Specialization(guards = "isAnyType(expectedType)")
    boolean doAnyType(Object expectedType, Object payload) {
      return true;
    }

    @Specialization
    boolean doLongCheck(Type expectedType, long payload) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getInteger(), expectedType);
    }

    @Specialization
    boolean doDoubleCheck(Type expectedType, double payload) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getDecimal(), expectedType);
    }

    @Specialization
    boolean doBigIntegerCheck(Type expectedType, EnsoBigInteger value) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getInteger(), expectedType);
    }

    @Specialization
    boolean doUnresolvedSymbol(Type expectedType, UnresolvedSymbol value) {
      var funTpe = EnsoContext.get(this).getBuiltins().function();
      return expectedType == funTpe;
    }

    @ExplodeLoop
    private boolean checkParentTypes(Type actual, Type expected) {
      var ctx = EnsoContext.get(this);
      for (var tpe : actual.allTypes(ctx)) {
        if (tpe == expected) {
          return true;
        }
      }
      return false;
    }

    @Specialization(guards = {"!isArrayType(expectedType)", "!isAnyType(expectedType)"})
    boolean doType(
        Type expectedType,
        Object payload,
        @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types) {
      return typeAndCheck(payload, expectedType, typeOfNode, isSameObject, profile);
    }

    @Specialization(
        guards = {
          "isArrayType(expectedType)",
          "interop.hasArrayElements(payload)",
        })
    public boolean doArrayViaType(
        Object expectedType,
        Object payload,
        @CachedLibrary(limit = "3") InteropLibrary interop,
        @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types) {
      return EnsoContext.get(this).getBuiltins().array() == types.getType(payload);
    }

    @Fallback
    boolean doOther(Object expectedType, Object payload) {
      return false;
    }

    boolean isAnyType(Object expectedType) {
      return EnsoContext.get(this).getBuiltins().any() == expectedType;
    }

    boolean isArrayType(Object expectedType) {
      return EnsoContext.get(this).getBuiltins().array() == expectedType;
    }
  }

  abstract static class Untyped extends Node {
    private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
    private @Child TypeOfNode typeOfNode = TypeOfNode.build();
    private final CountingConditionProfile profile = CountingConditionProfile.create();

    abstract boolean execute(Object expectedType, Object payload);

    @Specialization(
        guards = {
          "interop.isMetaObject(expectedType)",
          "isMetaInstance(interop, expectedType, payload)"
        })
    boolean doPolyglotType(
        Object expectedType, Object payload, @CachedLibrary(limit = "3") InteropLibrary interop) {
      return true;
    }

    static boolean isMetaInstance(InteropLibrary interop, Object expectedType, Object payload) {
      try {
        return interop.isMetaInstance(expectedType, payload);
      } catch (UnsupportedMessageException ex) {
        return false;
      }
    }

    @Fallback
    public boolean doOther(Object expectedType, Object payload) {
      return typeAndCheck(payload, expectedType, typeOfNode, isSameObject, profile);
    }
  }
}
