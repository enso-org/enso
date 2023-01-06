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
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.profiles.ValueProfile;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Meta",
    name = "atom_with_hole_builtin",
    description = "Creates a new atom with given constructor and fields.",
    autoRegister = false)
public abstract class AtomWithAHoleNode extends Node {

  static AtomWithAHoleNode build() {
    return AtomWithAHoleNodeGen.create();
  }

  abstract Object execute(VirtualFrame frame, Object factory);

  static InvokeCallableNode callWithHole() {
    return InvokeCallableNode.build(
      new CallArgumentInfo[] {new CallArgumentInfo()},
      InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
      InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  @Specialization
  Object doExecute(
    VirtualFrame frame,
    Object factory,
    @Cached("callWithHole()") InvokeCallableNode iop,
    @Cached SwapAtomFieldNode swapNode
  ) {
    var ctx = EnsoContext.get(this);
    var lazy = new HoleInAtom();
    var result = iop.execute(factory, frame, State.create(ctx), new Object[] { lazy });
    if (result instanceof Atom atom) {
      var index = swapNode.findHoleIndex(atom, lazy);
      if (index >= 0) {
        var function = swapNode.createFn(lazy);
        lazy.init(atom, index, function);
        return lazy;
      }
    }
    throw new PanicException(ctx.getBuiltins().error().makeUninitializedStateError(result), this);
  }

  @ExportLibrary(InteropLibrary.class)
  static final class HoleInAtom implements TruffleObject {
    Atom result;
    int index;
    Function function;

    HoleInAtom() {
    }

    void init(Atom result, int index, Function function) {
      this.result = result;
      this.index = index;
      this.function = function;
    }

    @ExportMessage boolean hasMembers() {
        return true;
    }

    @ExportMessage boolean isMemberReadable(String member) {
        return switch (member) {
            case "value", "fill" -> true;
            default -> false;
        };
    }

    @ExportMessage Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
        return new Array("value", "fill");
    }

    @ExportMessage
    Object readMember(String name) throws UnknownIdentifierException {
      if ("value".equals(name)) {
        return result;
      }
      if ("fill".equals(name)) {
        return function;
      }
      throw UnknownIdentifierException.create(name);
    }

    @ExportMessage
    String toDisplayString(boolean pure) {
      return "Meta.atom_with_hole";
    }
  }
  static final class SwapAtomFieldNode extends RootNode {
    private final FunctionSchema schema;
    private final ValueProfile sameAtom = ValueProfile.createClassProfile();
    @CompilerDirectives.CompilationFinal
    private int lastIndex = -1;

    private SwapAtomFieldNode() {
      super(null);
      this.schema = new FunctionSchema(FunctionSchema.CallerFrameAccess.NONE, new ArgumentDefinition[]{
        new ArgumentDefinition(0, "lazy", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "value", ArgumentDefinition.ExecutionMode.EXECUTE)
      }, new boolean[]{
        true, false
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

    Function createFn(HoleInAtom lazy) {
      var preArgs = new Object[]{lazy, null};
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
      if (args[0] instanceof HoleInAtom lazy) {
        var fields = lazy.result.getFields();
        var newValue = args[1];
        if (fields[lazy.index] == lazy) {
          fields[lazy.index] = newValue;
        }
        return newValue;
      }
      return EnsoContext.get(this).getBuiltins().nothing();
    }
  }
}
