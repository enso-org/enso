package org.enso.interpreter.node;

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
import org.enso.interpreter.node.BinaryOperatorNodeFactory.DoThatConversionNodeGen;
import org.enso.interpreter.node.callable.InteropConversionCallNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

final class BinaryOperatorNode extends ExpressionNode {
  private @Child ExpressionNode left;
  private @Child ExpressionNode right;
  private @Child ExpressionNode body;
  private @Child DoThatConversionNode thatConversion;

  BinaryOperatorNode(ExpressionNode left, ExpressionNode right, ExpressionNode body) {
    this.body = body;
    this.left = left;
    this.right = right;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    left.executeGeneric(frame);
    try {
      right.executeGeneric(frame);
    } catch (PanicException ex) {
      var ctx = EnsoContext.get(this);
      if (ex.getPayload() instanceof Atom error && ctx.getBuiltins().error().isTypeError(error)) {
        if (getRootNode() instanceof MethodRootNode methodRoot
            && ex.getLocation() != null
            && ex.getLocation().getRootNode() == methodRoot) {
          if (thatConversion == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            thatConversion = insert(DoThatConversionNode.create());
          }
          var name = methodRoot.getMethodName();
          var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());

          var result = thatConversion.executeThatConversion(frame, name, args[0], args[1]);
          if (result != null) {
            return result;
          } else {
            throw ex;
          }
        }
      }
      throw ex;
    }
    return body.executeGeneric(frame);
  }

  /**
   * A node that checks the type of {@code that} argument, performs conversion of {@code self} to
   * {@code that} type, if it contains the requested symbol implementation, and executes that method
   * instead of the original operator.
   */
  @GenerateUncached
  abstract static class DoThatConversionNode extends Node {
    @NeverDefault
    static DoThatConversionNode getUncached() {
      return DoThatConversionNodeGen.getUncached();
    }

    @NeverDefault
    static DoThatConversionNode create() {
      return DoThatConversionNodeGen.create();
    }

    /**
     * @return {code null} if the conversion makes no sense or result of doing the conversion an
     *     executing the {@link #symbol}
     */
    abstract Object executeThatConversion(
        VirtualFrame frame, String symbol, Object self, Object that);

    static Type findType(TypeOfNode typeOfNode, Object obj) {
      var rawType = typeOfNode.execute(obj);
      return rawType instanceof Type type ? type : null;
    }

    static Type findTypeUncached(Object obj) {
      return findType(TypeOfNode.getUncached(), obj);
    }

    final Function findSymbol(String symbol, Type type) {
      var unresolved = UnresolvedSymbol.build(symbol, type.getDefinitionScope());
      var found = unresolved.resolveFor(this, type);
      if (found != null) {
        return found.getLeft();
      } else {
        return null;
      }
    }

    @Specialization(
        limit = "3",
        guards = {
          "cachedSymbol.equals(symbol)",
          "!types.hasSpecialDispatch(that)",
          "selfType != null",
          "thatType != null",
          "symbolFn != null",
          "thatType == findType(typeOfNode, that)"
        })
    final Object doThatConversionCached(
        VirtualFrame frame,
        String symbol,
        Object self,
        Object that,
        @Cached("symbol") String cachedSymbol,
        @Shared("typeOf") @Cached TypeOfNode typeOfNode,
        @CachedLibrary(limit = "3") TypesLibrary types,
        @Cached(value = "findType(typeOfNode, self)", uncached = "findTypeUncached(self)")
            Type selfType,
        @Cached(value = "findType(typeOfNode, that)", uncached = "findTypeUncached(that)")
            Type thatType,
        @Cached(allowUncached = true, value = "findSymbol(cachedSymbol, thatType)")
            Function symbolFn,
        @Shared("convert") @Cached InteropConversionCallNode convertNode,
        @Shared("invoke") @Cached(allowUncached = true, value = "buildWithArity(2)")
            InvokeFunctionNode invokeNode) {
      return doDispatch(frame, self, that, thatType, symbolFn, convertNode, invokeNode);
    }

    @Specialization(replaces = "doThatConversionCached")
    final Object doThatConversionUncached(
        VirtualFrame frame,
        String symbol,
        Object self,
        Object that,
        @Shared("typeOf") @Cached TypeOfNode typeOfNode,
        @Shared("convert") @Cached InteropConversionCallNode convertNode,
        @Shared("invoke") @Cached(allowUncached = true, value = "buildWithArity(2)")
            InvokeFunctionNode invokeNode) {
      if (that instanceof EnsoMultiValue multi) {
        for (var thatType : multi.allTypes()) {
          var fn = findSymbol(symbol, thatType);
          if (fn != null) {
            var result =
                doDispatch(
                    frame, self, multi.castTo(thatType), thatType, fn, convertNode, invokeNode);
            if (result != null) {
              return result;
            }
          }
        }
      } else {
        var thatType = findType(typeOfNode, that);
        if (thatType != null) {
          var fn = findSymbol(symbol, thatType);
          if (fn != null) {
            var result = doDispatch(frame, self, that, thatType, fn, convertNode, invokeNode);
            if (result != null) {
              return result;
            }
          }
        }
      }
      return null;
    }

    private Object doDispatch(
        VirtualFrame frame,
        Object self,
        Object that,
        Type thatType,
        Function symbolFn,
        InteropConversionCallNode convertNode,
        InvokeFunctionNode invokeNode)
        throws PanicException {
      var convert = UnresolvedConversion.build(thatType.getDefinitionScope());

      var ctx = EnsoContext.get(this);
      var state = State.create(ctx);
      try {
        var selfAsThat = convertNode.execute(convert, state, new Object[] {thatType, self});
        var result = invokeNode.execute(symbolFn, frame, state, new Object[] {selfAsThat, that});
        return result;
      } catch (ArityException ex) {
        var assertsOn = false;
        assert assertsOn = true;
        if (assertsOn) {
          throw new AssertionError("Unexpected arity exception", ex);
        }
        return null;
      } catch (PanicException ex) {
        if (ctx.getBuiltins().error().isNoSuchConversionError(ex.getPayload())) {
          return null;
        }
        throw ex;
      }
    }
  }
}
