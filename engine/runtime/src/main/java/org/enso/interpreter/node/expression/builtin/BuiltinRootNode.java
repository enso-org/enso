package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.ControlFlowException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.pkg.QualifiedName;

/** Root node for use by all the builtin functions. */
@NodeInfo(shortName = "BuiltinRoot", description = "Root node for builtin functions.")
public abstract class BuiltinRootNode extends RootNode {
  private QualifiedName moduleName;
  private QualifiedName typeName;

  protected BuiltinRootNode(EnsoLanguage language) {
    super(language);
  }

  /** Get the module name where the builtin is defined. */
  public QualifiedName getModuleName() {
    return moduleName;
  }

  /** Set the module name where the builtin is defined. */
  public void setModuleName(QualifiedName moduleName) {
    this.moduleName = moduleName;
  }

  /** Get the type name of the builtin. */
  public QualifiedName getTypeName() {
    return typeName;
  }

  /** Set the type name of the builtin. */
  public void setTypeName(QualifiedName typeName) {
    this.typeName = typeName;
  }

  /**
   * Executes this node's logic, returning a pair of return value and the new state.
   *
   * @param frame current execution frame
   * @return the result value of executing the logic.
   */
  @Override
  public abstract Object execute(VirtualFrame frame);

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public abstract String getName();

  protected static final class ReturnValue extends ControlFlowException {
    private final TruffleObject value;

    private ReturnValue(TruffleObject value) {
      this.value = value;
    }

    public TruffleObject get() {
      return value;
    }
  }

  protected abstract static class ArgNode extends Node {
    private final boolean isSelf;
    private final boolean isArray;
    private final boolean requiresCast;
    private final boolean checkErrors;
    private final boolean checkPanicSentinel;
    private final boolean checkWarnings;
    @CompilerDirectives.CompilationFinal private Type ensoType;

    ArgNode(
        boolean isSelf,
        boolean isArray,
        boolean requiresCast,
        boolean checkErrors,
        boolean checkPanicSentinel,
        boolean checkWarnings) {
      this.isSelf = isSelf;
      this.isArray = isArray;
      this.requiresCast = requiresCast;
      this.checkErrors = checkErrors;
      this.checkPanicSentinel = checkPanicSentinel;
      this.checkWarnings = checkWarnings;
    }

    @SuppressWarnings("unchecked")
    public final <T> T processArgument(Class<T> type, Object value) throws ReturnValue {
      if (checkErrors && value instanceof DataflowError err) {
        throw new ReturnValue(err);
      }
      var ctx = EnsoContext.get(this);
      if (this.ensoType == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        var builtin = ctx.getBuiltins().getByRepresentationType(type);
        if (builtin == null) {
          System.err.println("found no builtin for " + type);
          this.ensoType = ctx.getBuiltins().any();
        } else {
          this.ensoType = builtin.getType();
        }
      }
      assert value != null;
      var conv = executeConversion(value);
      if (conv == null) {
        CompilerDirectives.transferToInterpreter();
        var err = ctx.getBuiltins().error().makeTypeError(this.ensoType, value, type.getName());
        throw new PanicException(err, this);
      }
      return type.cast(conv);
    }

    abstract Object executeConversion(Object obj);

    @Specialization
    final Object extractMultiValue(EnsoMultiValue emv, @Cached EnsoMultiValue.CastToNode castTo) {
      var extracted = castTo.findTypeOrNull(ensoType, emv, false, false);
      return extracted;
    }

    @Fallback
    final Object justReturnIt(Object obj) {
      return obj;
    }

    public static ArgNode create(
        boolean isSelf,
        boolean isArray,
        boolean requiresCast,
        boolean checkErrors,
        boolean checkPanicSentinel,
        boolean checkWarnings) {
      return BuiltinRootNodeFactory.ArgNodeGen.create(
          isSelf, isArray, requiresCast, checkErrors, checkPanicSentinel, checkWarnings);
    }

    /*
    if (!arg.requiresCast()) {
      generateUncastedArgumentRead(out, arg, argsArray);
    } else if (arg.isSelf()) {
      generateUncheckedArgumentRead(out, arg, argsArray);
    } else if (arg.isArray()) {
      generateUncheckedArrayCast(out, arg, argsArray);
    } else {
      generateCheckedArgumentRead(out, arg, argsArray);
    }

      */
  }
}
