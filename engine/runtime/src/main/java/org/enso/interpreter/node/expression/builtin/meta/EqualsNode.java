package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.callable.InteropConversionCallNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.StructsLibrary;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.State;

public final class EqualsNode extends Node {
  @Child private EqualsSimpleNode node;
  @Child private TypeOfNode types;
  @Child private WithConversionNode convert;

  private static final EqualsNode UNCACHED =
      new EqualsNode(EqualsSimpleNodeGen.getUncached(), TypeOfNode.getUncached(), true);

  private EqualsNode(EqualsSimpleNode node, TypeOfNode types, boolean uncached) {
    this.node = node;
    this.types = types;
    if (uncached) {
      convert = EqualsNodeFactory.WithConversionNodeGen.getUncached();
    }
  }

  @NeverDefault
  static EqualsNode build() {
    return create();
  }

  @NeverDefault
  public static EqualsNode create() {
    return new EqualsNode(EqualsSimpleNode.build(), TypeOfNode.create(), false);
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
  public EqualsAndInfo execute(VirtualFrame frame, Object self, Object other) {
    var areEqual = node.execute(frame, self, other);
    if (!areEqual.isTrue()) {
      var selfType = types.execute(self);
      var otherType = types.execute(other);
      if (selfType != otherType) {
        if (convert == null) {
          CompilerDirectives.transferToInterpreter();
          convert = insert(WithConversionNode.create());
        }
        return convert.executeWithConversion(frame, other, self);
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
    abstract EqualsAndInfo executeWithConversion(VirtualFrame frame, Object self, Object that);

    static Type findType(TypeOfNode typeOfNode, Object obj) {
      var rawType = typeOfNode.execute(obj);
      return rawType instanceof Type type ? type : null;
    }

    static Type findTypeUncached(Object obj) {
      return findType(TypeOfNode.getUncached(), obj);
    }

    private static boolean isDefinedIn(ModuleScope scope, Function fn) {
      if (fn.getCallTarget().getRootNode() instanceof EnsoRootNode ensoRoot) {
        return ensoRoot.getModuleScope().getModule() == scope.getModule();
      } else {
        return false;
      }
    }

    @CompilerDirectives.TruffleBoundary
    private static Object convertor(EnsoContext ctx, Function convFn, Object value) {
      var argSchema = new CallArgumentInfo[] {new CallArgumentInfo(), new CallArgumentInfo()};
      var node =
          InvokeFunctionNode.build(
              argSchema, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.EXECUTE);
      var state = State.create(ctx);
      var by =
          node.execute(convFn, null, state, new Object[] {ctx.getBuiltins().comparable(), value});
      if (by instanceof Atom atom
          && atom.getConstructor() == ctx.getBuiltins().comparable().getBy()) {
        var structs = StructsLibrary.getUncached();
        return structs.getField(atom, 1);
      } else {
        return null;
      }
    }

    /**
     * @return {@code null} if no conversion found
     */
    Boolean findConversions(Type selfType, Type thatType, Object self, Object that) {
      if (selfType == null || thatType == null) {
        return null;
      }
      var ctx = EnsoContext.get(this);

      if (findConversionImpl(ctx, selfType, thatType, self, that)) {
        return false;
      } else {
        if (findConversionImpl(ctx, thatType, selfType, that, self)) {
          return true;
        } else {
          return null;
        }
      }
    }

    private static boolean findConversionImpl(
        EnsoContext ctx, Type selfType, Type thatType, Object self, Object that) {
      var selfScope = selfType.getDefinitionScope();
      var comparableType = ctx.getBuiltins().comparable().getType();

      var fromSelfType =
          UnresolvedConversion.build(selfScope).resolveFor(ctx, comparableType, selfType);
      var fromThatType =
          UnresolvedConversion.build(selfScope).resolveFor(ctx, comparableType, thatType);
      var betweenBoth = UnresolvedConversion.build(selfScope).resolveFor(ctx, selfType, thatType);

      if (isDefinedIn(selfScope, fromSelfType) && isDefinedIn(selfScope, fromThatType)) {
        var c1 = convertor(ctx, fromSelfType, self);
        var c2 = convertor(ctx, fromThatType, that);
        if (c1 == c2 && c1 != null && betweenBoth != null) {
          return true;
        }
      }
      return false;
    }

    @Specialization(
        limit = "10",
        guards = {
          "selfType != null",
          "thatType != null",
          "selfType == findType(typeOfNode, self)",
          "thatType == findType(typeOfNode, that)"
        })
    final EqualsAndInfo doConversionCached(
        VirtualFrame frame,
        Object self,
        Object that,
        @Shared("typeOf") @Cached TypeOfNode typeOfNode,
        @Cached(value = "findType(typeOfNode, self)", uncached = "findTypeUncached(self)")
            Type selfType,
        @Cached(value = "findType(typeOfNode, that)", uncached = "findTypeUncached(that)")
            Type thatType,
        @Cached("findConversions(selfType, thatType, self, that)") Boolean convert,
        @Shared("convert") @Cached InteropConversionCallNode convertNode,
        @Shared("invoke") @Cached(allowUncached = true) EqualsSimpleNode equalityNode) {
      if (convert == null) {
        return EqualsAndInfo.FALSE;
      }
      if (convert) {
        return doDispatch(frame, that, self, thatType, convertNode, equalityNode);
      } else {
        return doDispatch(frame, self, that, selfType, convertNode, equalityNode);
      }
    }

    @Specialization(replaces = "doConversionCached")
    final EqualsAndInfo doConversionUncached(
        VirtualFrame frame,
        Object self,
        Object that,
        @Shared("typeOf") @Cached TypeOfNode typeOfNode,
        @Shared("convert") @Cached InteropConversionCallNode convertNode,
        @Shared("invoke") @Cached(allowUncached = true) EqualsSimpleNode equalityNode) {
      var selfType = findType(typeOfNode, self);
      var thatType = findType(typeOfNode, that);
      var conv = findConversions(selfType, thatType, self, that);
      if (conv != null) {
        var result =
            conv
                ? doDispatch(frame, that, self, thatType, convertNode, equalityNode)
                : doDispatch(frame, self, that, selfType, convertNode, equalityNode);
        return result;
      }
      return EqualsAndInfo.FALSE;
    }

    private EqualsAndInfo doDispatch(
        VirtualFrame frame,
        Object self,
        Object that,
        Type selfType,
        InteropConversionCallNode convertNode,
        EqualsSimpleNode equalityNode)
        throws PanicException {
      var convert = UnresolvedConversion.build(selfType.getDefinitionScope());

      var ctx = EnsoContext.get(this);
      var state = State.create(ctx);
      try {
        var thatAsSelf = convertNode.execute(convert, state, new Object[] {selfType, that});
        var withInfo = equalityNode.execute(frame, self, thatAsSelf);
        var result = withInfo.isTrue();
        assert !result || assertHashCodeIsTheSame(that, thatAsSelf);
        return withInfo;
      } catch (ArityException ex) {
        var assertsOn = false;
        assert assertsOn = true;
        if (assertsOn) {
          throw new AssertionError("Unexpected arity exception", ex);
        }
        return EqualsAndInfo.FALSE;
      } catch (PanicException ex) {
        if (ctx.getBuiltins().error().isNoSuchConversionError(ex.getPayload())) {
          return EqualsAndInfo.FALSE;
        }
        throw ex;
      }
    }

    private boolean assertHashCodeIsTheSame(Object self, Object converted) {
      var selfHash = HashCodeNode.getUncached().execute(self);
      var convertedHash = HashCodeNode.getUncached().execute(converted);
      var ok = selfHash == convertedHash;
      if (!ok) {
        var msg =
            "Different hash code! Original "
                + self
                + "[#"
                + Long.toHexString(selfHash)
                + "] got converted to "
                + converted
                + "[#"
                + Long.toHexString(convertedHash)
                + "]";
        var ctx = EnsoContext.get(this);
        throw ctx.raiseAssertionPanic(this, msg, new AssertionError(msg));
      }
      return ok;
    }
  }
}
