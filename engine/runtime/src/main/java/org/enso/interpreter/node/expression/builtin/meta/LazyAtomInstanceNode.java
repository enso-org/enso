package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "lazy_atom",
    description = "Creates a new atom with given constructor and fields.",
    autoRegister = false)
public abstract class LazyAtomInstanceNode extends Node {

  static LazyAtomInstanceNode build() {
    return LazyAtomInstanceNodeGen.create();
  }

  abstract Vector execute(Object factory);

  @Specialization(guards = "iop.isExecutable(factory)")
  Vector doExecute(
    Object factory,
    @CachedLibrary(limit="3") InteropLibrary iop,
    @Cached CoerceArrayNode coerce,
    @Cached SwapAtomFieldNode swapNode
  ) {
    var ctx = EnsoContext.get(this);
    var lazy = new HoleInAtom();
    try {
      var r = iop.execute(factory, lazy);
      if (r instanceof Atom a) {
        for (int i = 0; i < a.getFields().length; i++) {
          if (a.getFields()[i] == lazy) {
            var function = swapNode.createFn(a, i, lazy);
            return Vector.fromArray(new Array(a, function));
          }
        }
      }
      throw new PanicException(ctx.getBuiltins().error().getInvalidArrayIndex(), this);
    } catch (UnsupportedTypeException ex) {
      throw raise(RuntimeException.class, ex);
    } catch (ArityException ex) {
      throw raise(RuntimeException.class, ex);
    } catch (UnsupportedMessageException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> type, Throwable t) throws E {
    throw (E)t;
  }

  @ExportLibrary(InteropLibrary.class)
  static final class HoleInAtom implements TruffleObject {
    HoleInAtom() {
    }

    @ExportMessage
    String toDisplayString(boolean pure) {
      return "Meta.lazy_atom";
    }
  }
  static final class SwapAtomFieldNode extends RootNode {
    private final FunctionSchema schema;

    private SwapAtomFieldNode() {
      super(null);
      this.schema = new FunctionSchema(FunctionSchema.CallerFrameAccess.NONE, new ArgumentDefinition[]{
        new ArgumentDefinition(0, "instance", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "lazy_index", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(2, "lazy", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(3, "value", ArgumentDefinition.ExecutionMode.EXECUTE)
      }, new boolean[]{
        true, true, true, false
      }, new CallArgumentInfo[0]);
    }

    static SwapAtomFieldNode create() {
      return new SwapAtomFieldNode();
    }

    Function createFn(Atom atom, int index, HoleInAtom lazy) {
      var preArgs = new Object[]{atom, index, lazy, null};
      return new Function(
              getCallTarget(),
              null,
              schema,
              preArgs,
              new Object[]{}
      );
    }

    @Override
    public Object execute(VirtualFrame frame) {
      var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
      if (args[0] instanceof Atom replace) {
        if (args[1] instanceof Number n) {
          var fields = replace.getFields();
          if (fields[n.intValue()] == args[2]) {
            fields[n.intValue()] = args[3];
          }
        }
      }
      return EnsoContext.get(this).getBuiltins().nothing();
    }
  }
}
