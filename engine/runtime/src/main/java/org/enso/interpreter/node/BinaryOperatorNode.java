package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.BinaryOperatorNodeFactory.DoThatConversionNodeGen;
import org.enso.interpreter.node.callable.InteropConversionCallNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

class BinaryOperatorNode extends ExpressionNode {
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
   * A node that checks the type of {@code that} argument and performs conversion of {@code self} to
   * {@code that} type, if it contains the requested symbol implementation.
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

    @NeverDefault
    static UnresolvedConversion buildConversion(Type type) {
      return UnresolvedConversion.build(type.getDefinitionScope());
    }

    @Specialization(
        limit = "3",
        guards = {
          "cachedSymbol.equals(symbol)",
          "selfType != null",
          "thatType != null",
          "symbolFn != null",
          "thatType == findType(typeOfNode, that)"
        })
    final Object doThatConversion(
        VirtualFrame frame,
        String symbol,
        Object self,
        Object that,
        @Cached("symbol") String cachedSymbol,
        @Cached TypeOfNode typeOfNode,
        @Cached(value = "findType(typeOfNode, self)", uncached = "findTypeUncached(self)")
            Type selfType,
        @Cached(value = "findType(typeOfNode, that)", uncached = "findTypeUncached(that)")
            Type thatType,
        @Cached(allowUncached = true, value = "findSymbol(cachedSymbol, thatType)")
            Function symbolFn,
        @Cached(allowUncached = true, value = "buildConversion(thatType)")
            UnresolvedConversion convert,
        @Cached InteropConversionCallNode convertNode,
        @Cached(allowUncached = true, value = "buildWithArity(2)") InvokeFunctionNode invokeNode) {
      var ctx = EnsoContext.get(this);
      var state = State.create(ctx);
      try {
        var selfAsThat = convertNode.execute(convert, state, new Object[] {thatType, self});
        var result = invokeNode.execute(symbolFn, frame, state, new Object[] {selfAsThat, that});
        return result;
      } catch (ArityException ex) {
        return null;
      }
    }

    @Specialization
    final Object doThatConversionSlow(VirtualFrame frame, String symbol, Object self, Object that) {
      return null;
    }
  }
}
