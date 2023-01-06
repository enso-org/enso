package org.enso.interpreter.node.expression.builtin.meta;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

import com.oracle.truffle.api.CompilerDirectives;
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

@BuiltinMethod(
    type = "Meta",
    name = "atom_with_hole_builtin",
    description = "Creates a new atom with given constructor and fields.",
    autoRegister = false)
public abstract class AtomWithAHoleNode extends Node {

  static AtomWithAHoleNode build() {
    return AtomWithAHoleNodeGen.create();
  }

  abstract Vector execute(Object factory);

  @Specialization(guards = "iop.isExecutable(factory)")
  Vector doExecute(
    Object factory,
    @CachedLibrary(limit="3") InteropLibrary iop,
    @Cached SwapAtomFieldNode swapNode
  ) {
    var ctx = EnsoContext.get(this);
    var lazy = new HoleInAtom();
    try {
      var r = iop.execute(factory, lazy);
      if (r instanceof Atom a) {
        var i = swapNode.findHoleIndex(a, lazy);
        if (i >= 0) {
          var function = swapNode.createFn(a, i, lazy);
          return Vector.fromArray(new Array(a, function));
        }
      }
      throw new PanicException(ctx.getBuiltins().error().makeUninitializedStateError(r), this);
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
      return "Meta.atom_with_hole";
    }
  }
  static final class SwapAtomFieldNode extends RootNode {
    private final FunctionSchema schema;
    @CompilerDirectives.CompilationFinal
    private int lastIndex = -1;

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

    int findHoleIndex(Atom atom, HoleInAtom lazy) {
      var arr = atom.getFields();
      if (lastIndex >= 0 && lastIndex < arr.length) {
        if (arr[lastIndex] == lazy) {
          return lastIndex;
        }
      }
      int index = findHoleIndexLoop(arr, lazy);
      if (index == -1) {
        return -1;
      }
      if (lastIndex == -1) {
        lastIndex = index;
        CompilerDirectives.transferToInterpreterAndInvalidate();
        return index;
      } else {
        if (lastIndex != -2) {
          CompilerDirectives.transferToInterpreterAndInvalidate();
          lastIndex = -2;
        }
      }
      return index;
    }

    @CompilerDirectives.TruffleBoundary
    private int findHoleIndexLoop(Object[] arr, HoleInAtom lazy) {
        for (int i = 0; i < arr.length; i++) {
          if (arr[i] == lazy) {
            return i;
          }
        }
        return -1;
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
        if (args[1] instanceof Integer n) {
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
