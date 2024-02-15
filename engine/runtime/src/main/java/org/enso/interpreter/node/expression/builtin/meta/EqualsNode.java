package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InteropConversionCallNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Any",
    name = "==",
    description =
        """
      Compares self with other object and returns True iff `self` is exactly the same as
      the other object, including all its transitively accessible properties or fields,
      False otherwise.

      Can handle arbitrary objects, including all foreign objects.

      Does not throw dataflow errors or panics.

      Note that this is different than `Meta.is_same_object`, which checks whether two
      references point to the same object on the heap. Moreover, `Meta.is_same_object`
      implies `Any.==` for all object with the exception of `Number.nan`.
      """)
public final class EqualsNode extends Node {
  @Child private EqualsSimpleNode node;
  @Child private TypeOfNode types;
  @Child private WithConversionNode convert1;
  @Child private WithConversionNode convert2;

  private static final EqualsNode UNCACHED =
      new EqualsNode(EqualsSimpleNodeGen.getUncached(), TypeOfNode.getUncached(), true);

  private EqualsNode(EqualsSimpleNode node, TypeOfNode types, boolean uncached) {
    this.node = node;
    this.types = types;
    if (uncached) {
      convert1 = EqualsNodeFactory.WithConversionNodeGen.getUncached();
      convert2 = convert1;
    }
  }

  @NeverDefault
  static EqualsNode build() {
    return create();
  }

  @NeverDefault
  public static EqualsNode create() {
    return new EqualsNode(EqualsSimpleNode.build(), TypeOfNode.build(), false);
  }

  @NeverDefault
  public static EqualsNode getUncached() {
    return UNCACHED;
  }

  /**
   * Compares two objects for equality. If the {@link EqualsSimpleNode simple check} fails, it tries
   * to convert first argument to the second one and compare again.
   *
   * @param frame the stack frame we are executing at
   * @param self the self object
   * @param other the other object
   * @return {@code true} if {@code self} and {@code that} seem equal
   */
  public boolean execute(
      VirtualFrame frame, @AcceptsError Object self, @AcceptsError Object other) {
    var areEqual = node.execute(frame, self, other);
    if (!areEqual) {
      var selfType = types.execute(self);
      var otherType = types.execute(other);
      if (selfType != otherType) {
        if (convert1 == null) {
          CompilerDirectives.transferToInterpreter();
          convert1 = insert(WithConversionNode.create());
          convert2 = insert(WithConversionNode.create());
        }
        return convert1.executeThatConversion(frame, other, self)
            || convert2.executeThatConversion(frame, self, other);
      }
    }
    return areEqual;
  }

  /**
   * A node that checks the type of {@code that} argument, performs conversion of {@code self} to
   * {@code that} type, and executes equality check again.
   */
  @GenerateUncached
  abstract static class WithConversionNode extends Node {

    @NeverDefault
    static WithConversionNode create() {
      return EqualsNodeFactory.WithConversionNodeGen.create();
    }

    /**
     * @return {code false} if the conversion makes no sense or result of equality check after doing
     *     the conversion
     */
    abstract boolean executeThatConversion(VirtualFrame frame, Object self, Object that);

    static Type findType(TypeOfNode typeOfNode, Object obj) {
      var rawType = typeOfNode.execute(obj);
      return rawType instanceof Type type ? type : null;
    }

    static Type findTypeUncached(Object obj) {
      return findType(TypeOfNode.getUncached(), obj);
    }

    @Specialization(
        limit = "3",
        guards = {
          "!types.hasSpecialDispatch(that)",
          "selfType != null",
          "thatType != null",
          "thatType == findType(typeOfNode, that)"
        })
    final boolean doThatConversionCached(
        VirtualFrame frame,
        Object self,
        Object that,
        @Shared("typeOf") @Cached TypeOfNode typeOfNode,
        @CachedLibrary(limit = "3") TypesLibrary types,
        @Cached(value = "findType(typeOfNode, self)", uncached = "findTypeUncached(self)")
            Type selfType,
        @Cached(value = "findType(typeOfNode, that)", uncached = "findTypeUncached(that)")
            Type thatType,
        @Shared("convert") @Cached InteropConversionCallNode convertNode,
        @Shared("invoke") @Cached(allowUncached = true) EqualsSimpleNode equalityNode) {
      return doDispatch(frame, self, that, thatType, convertNode, equalityNode);
    }

    @Specialization(replaces = "doThatConversionCached")
    final boolean doThatConversionUncached(
        VirtualFrame frame,
        Object self,
        Object that,
        @Shared("typeOf") @Cached TypeOfNode typeOfNode,
        @Shared("convert") @Cached InteropConversionCallNode convertNode,
        @Shared("invoke") @Cached(allowUncached = true) EqualsSimpleNode equalityNode) {
      /*
      if (that instanceof EnsoMultiValue multi) {
        for (var thatType : multi.allTypes()) {
          var fn = findSymbol(symbol, thatType);
          if (fn != null) {
            var result =
                doDispatch(
                    frame, self, multi.castTo(thatType), thatType, fn, convertNode, equalityNode);
            if (result != null) {
              return result;
            }
          }
        }
      } else */
      {
        var thatType = findType(typeOfNode, that);
        if (thatType != null) {
          var result = doDispatch(frame, self, that, thatType, convertNode, equalityNode);
          return result;
        }
      }
      return false;
    }

    private boolean doDispatch(
        VirtualFrame frame,
        Object self,
        Object that,
        Type thatType,
        InteropConversionCallNode convertNode,
        EqualsSimpleNode equalityNode)
        throws PanicException {
      var convert = UnresolvedConversion.build(thatType.getDefinitionScope());

      var ctx = EnsoContext.get(this);
      var state = State.create(ctx);
      try {
        var selfAsThat = convertNode.execute(convert, state, new Object[] {thatType, self});
        var result = equalityNode.execute(frame, selfAsThat, that);
        return result;
      } catch (ArityException ex) {
        var assertsOn = false;
        assert assertsOn = true;
        if (assertsOn) {
          throw new AssertionError("Unexpected arity exception", ex);
        }
        return false;
      } catch (PanicException ex) {
        if (ctx.getBuiltins().error().isNoSuchConversionError(ex.getPayload())) {
          return false;
        }
        throw ex;
      }
    }
  }
}
