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
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

/** An implementation of the payload check against the expected panic type. */
@NodeInfo(shortName = "IsValueOfTypeNode")
public abstract class IsValueOfTypeNode extends Node {
  IsValueOfTypeNode() {}

  public static IsValueOfTypeNode build() {
    return IsValueOfTypeNodeGen.create();
  }

  /**
   * @param expectedType the type to check
   * @param obj the object to check
   * @param includeExtraTypes specify {@code false} to return only <em>types value has already been
   *     cast to</em>, specify {@code true} to return all <em>types value can be cast to</em>
   */
  public abstract boolean execute(Object expectedType, Object obj, boolean includeExtraTypes);

  @Specialization(guards = {"types.hasType(payload)"})
  boolean doTyped(
      Object expectedType,
      Object payload,
      boolean allTypes,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached Typed typed) {
    return typed.execute(expectedType, payload, allTypes);
  }

  @Specialization(guards = {"!types.hasType(payload)"})
  boolean doPolyglot(
      Object expectedType,
      Object payload,
      boolean allTypes,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached Untyped untyped) {
    return untyped.execute(expectedType, payload, allTypes);
  }

  private static boolean typeAndCheck(
      Object payload,
      Object expectedMeta,
      boolean allTypes,
      TypeOfNode typeOfNode,
      IsSameObjectNode isSameObject,
      CountingConditionProfile isSameObjectProfile) {
    if (expectedMeta instanceof Type expectedType) {
      var arr = typeOfNode.findAllTypesOrNull(payload, allTypes);
      if (arr == null) {
        return false;
      }
      for (var tpeOfPayload : arr) {
        if (isSameObjectProfile.profile(isSameObject.execute(expectedType, tpeOfPayload))) {
          return true;
        } else {
          var ctx = EnsoContext.get(typeOfNode);
          for (var superTpe : tpeOfPayload.allTypes(ctx)) {
            boolean testSuperTpe = isSameObject.execute(expectedType, superTpe);
            if (testSuperTpe) {
              return true;
            }
          }
        }
      }
    } else {
      var tpe = typeOfNode.findTypeOrError(payload);
      return isSameObject.execute(expectedMeta, tpe);
    }
    return false;
  }

  abstract static class Typed extends Node {
    private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
    private @Child TypeOfNode typeOfNode = TypeOfNode.create();
    private final CountingConditionProfile profile = CountingConditionProfile.create();

    abstract boolean execute(Object expectedType, Object payload, boolean allTypes);

    @Specialization(guards = "isAnyType(expectedType)")
    boolean doAnyType(Object expectedType, Object payload, boolean allTypes) {
      return true;
    }

    @Specialization
    boolean doLongCheck(Type expectedType, long payload, boolean allTypes) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getInteger(), expectedType);
    }

    @Specialization
    boolean doDoubleCheck(Type expectedType, double payload, boolean allTypes) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getFloat(), expectedType);
    }

    @Specialization
    boolean doBigIntegerCheck(Type expectedType, EnsoBigInteger value, boolean allTypes) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getInteger(), expectedType);
    }

    @Specialization
    boolean doUnresolvedSymbol(Type expectedType, UnresolvedSymbol value, boolean allTypes) {
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
        boolean allTypes,
        @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types) {
      return typeAndCheck(payload, expectedType, allTypes, typeOfNode, isSameObject, profile);
    }

    @Specialization(
        guards = {
          "isArrayType(expectedType)",
          "interop.hasArrayElements(payload)",
        })
    public boolean doArrayViaType(
        Object expectedType,
        Object payload,
        boolean allTypes,
        @CachedLibrary(limit = "3") InteropLibrary interop,
        @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types) {
      return EnsoContext.get(this).getBuiltins().array() == types.getType(payload);
    }

    @Fallback
    boolean doOther(Object expectedType, Object payload, boolean allTypes) {
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
    private @Child TypeOfNode typeOfNode = TypeOfNode.create();
    private final CountingConditionProfile profile = CountingConditionProfile.create();

    abstract boolean execute(Object expectedType, Object payload, boolean allTypes);

    @Specialization(
        guards = {
          "interop.isMetaObject(expectedType)",
          "isMetaInstance(interop, expectedType, payload)"
        })
    boolean doPolyglotType(
        Object expectedType,
        Object payload,
        boolean allTypes,
        @CachedLibrary(limit = "3") InteropLibrary interop) {
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
    public boolean doOther(Object expectedType, Object payload, boolean allTypes) {
      return typeAndCheck(payload, expectedType, allTypes, typeOfNode, isSameObject, profile);
    }
  }
}
