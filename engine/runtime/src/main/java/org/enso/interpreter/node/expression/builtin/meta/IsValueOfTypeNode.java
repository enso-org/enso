package org.enso.interpreter.node.expression.builtin.meta;

import org.enso.interpreter.epb.runtime.PolyglotExceptionProxy;
import org.enso.interpreter.epb.runtime.PolyglotProxy;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.type.TypesGen;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;

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
      @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached Typed typed) {
    return typed.execute(expectedType, payload);
  }

  @Specialization(guards = {"!types.hasType(payload)"})
  boolean doPolyglot(
      Object expectedType,
      Object payload,
      @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached Untyped typed) {
    return typed.execute(expectedType, payload);
  }

  abstract static class Typed extends Node {
    private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
    private @Child TypeOfNode typeOfNode = TypeOfNode.build();
    private final ConditionProfile profile = ConditionProfile.createCountingProfile();

    abstract boolean execute(Object expectedType, Object payload);

    @Specialization(guards = "isAnyType(expectedType)")
    boolean doAnyType(Object expectedType, Object payload) {
      return true;
    }

    @Specialization
    boolean doLongCheck(Type expectedType, long payload) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getSmallInteger(), expectedType);
    }

    @Specialization
    boolean doDoubleCheck(Type expectedType, double payload) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getDecimal(), expectedType);
    }

    @Specialization
    boolean doBigIntegerCheck(Type expectedType, EnsoBigInteger value) {
      var numbers = EnsoContext.get(this).getBuiltins().number();
      return checkParentTypes(numbers.getBigInteger(), expectedType);
    }

    @Specialization
    boolean doUresolvedSymbol(Type expectedType, UnresolvedSymbol value) {
      var funTpe = EnsoContext.get(this).getBuiltins().function();
      return expectedType == funTpe;
    }

    @ExplodeLoop
    private boolean checkParentTypes(Type actual, Type expected) {
      for (; ; ) {
        if (actual == null) {
          return false;
        }
        if (actual == expected) {
          return true;
        }
        actual = actual.getSupertype();
      }
    }

    @Specialization(guards = {"!isArrayType(expectedType)", "!isAnyType(expectedType)"})
    boolean doType(
        Type expectedType, Object payload, @CachedLibrary(limit = "3") TypesLibrary types) {
      Object tpeOfPayload = typeOfNode.execute(payload);
      if (profile.profile(isSameObject.execute(expectedType, tpeOfPayload))) {
        return true;
      } else if (TypesGen.isType(tpeOfPayload)) {
        Type tpe = TypesGen.asType(tpeOfPayload);
        Type superTpe = tpe.getSupertype();

        while (tpe != superTpe && superTpe != null) {
          boolean testSuperTpe = isSameObject.execute(expectedType, superTpe);
          if (testSuperTpe) {
            return true;
          }
          tpe = superTpe;
          superTpe = superTpe.getSupertype();
        }
      }
      return false;
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
        @CachedLibrary(limit = "3") TypesLibrary types) {
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

    abstract boolean execute(Object expectedType, Object payload);

    @Specialization(
        guards = {
          "isArrayType(expectedType)",
          "interop.hasArrayElements(payload)",
        })
    boolean doArray(
        Object expectedType, Object payload, @CachedLibrary(limit = "3") InteropLibrary interop) {
      return true;
    }

    @Specialization(guards = {"isMapType(expectedType, iop)", "iop.hasHashEntries(payload)"})
    boolean doMap(
        Type expectedType, Object payload, @CachedLibrary(limit = "3") InteropLibrary iop) {
      return true;
    }

    @Specialization
    boolean doPolyglotProxy(Object expectedType, PolyglotProxy proxy) {
      Object tpeOfPayload = typeOfNode.execute(proxy);
      return isSameObject.execute(expectedType, tpeOfPayload);
    }

    @Specialization(guards = "!isAnyType(expectedType)")
    boolean doPolyglotExceptionProxy(Object expectedType, PolyglotExceptionProxy proxy) {
      Object tpeOfPayload = typeOfNode.execute(proxy.getDelegate());
      return isSameObject.execute(expectedType, tpeOfPayload);
    }

    @Specialization(
        guards = {
          "!isArrayType(expectedType)",
          "!isMapType(expectedType, interop)",
          "interop.isMetaObject(expectedType)"
        })
    boolean doPolyglotType(
        Object expectedType, Object payload, @CachedLibrary(limit = "3") InteropLibrary interop) {
      try {
        return interop.isMetaInstance(expectedType, payload);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(e);
      }
    }

    static boolean isMapType(Object type, Node node) {
      var ctx = EnsoContext.get(node);
      return type == ctx.getBuiltins().map();
    }

    boolean isAnyType(Object expectedType) {
      return EnsoContext.get(this).getBuiltins().any() == expectedType;
    }

    boolean isArrayType(Object expectedType) {
      return EnsoContext.get(this).getBuiltins().array() == expectedType;
    }

    @Fallback
    public boolean doOther(Object expectedType, Object payload) {
      return false;
    }
  }
}
