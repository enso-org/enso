package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertAllNode;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.warning.AppendWarningNode;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
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

  protected static final class ArgContext {
    private TruffleObject returnValue;
    private EnsoHashMap warnings;

    public ArgContext() {}

    public TruffleObject getReturnValue() {
      return returnValue;
    }

    public boolean hasWarnings() {
      return this.warnings != null;
    }

    private void addWarnings(
        VirtualFrame frame, HashMapInsertAllNode insertNode, EnsoHashMap newWarnings) {
      if (this.warnings == null) {
        this.warnings = newWarnings;
      } else {
        int maxWarnings = EnsoContext.get(insertNode).getWarningsLimit();
        this.warnings = insertNode.executeInsertAll(frame, this.warnings, newWarnings, maxWarnings);
      }
    }
  }

  protected abstract static class ArgNode extends Node {
    private static final byte IS_SELF = 0x01;
    private static final byte REQUIRES_CAST = 0x04;
    private static final byte CHECK_ERRORS = 0x08;
    private static final byte CHECK_PANIC_SENTINEL = 0x10;
    private static final byte CHECK_WARNINGS = 0x20;
    private final byte flags;
    @CompilerDirectives.CompilationFinal private Type ensoType;

    @Child private WarningsLibrary warnings;
    @Child private AppendWarningNode appendWarningNode;
    @Child private HashMapInsertAllNode mapInsertAllNode;

    private final BranchProfile errorsTaken = BranchProfile.create();
    private final BranchProfile sentinelTaken = BranchProfile.create();

    ArgNode(byte flags) {
      this.flags = flags;
      if (is(CHECK_WARNINGS)) {
        this.warnings = WarningsLibrary.getFactory().createDispatched(5);
      }
    }

    private boolean is(byte what) {
      return (flags & what) != 0;
    }

    @SuppressWarnings("unchecked")
    public final <T> T processArgument(
        VirtualFrame frame, Class<T> type, Object value, ArgContext context) {
      assert value != null;
      if (is(CHECK_ERRORS) && value instanceof DataflowError err) {
        errorsTaken.enter();
        context.returnValue = err;
        return null;
      }
      if (is(CHECK_PANIC_SENTINEL) && value instanceof PanicSentinel sentinel) {
        sentinelTaken.enter();
        throw sentinel;
      }
      if (warnings != null) {
        if (warnings.hasWarnings(value)) {
          if (mapInsertAllNode == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            this.mapInsertAllNode = insert(HashMapInsertAllNode.build());
          }
          try {
            context.addWarnings(frame, mapInsertAllNode, warnings.getWarnings(value, false));
            value = warnings.removeWarnings(value);
          } catch (UnsupportedMessageException ex) {
            throw raise(RuntimeException.class, ex);
          }
        }
      }
      if (is(REQUIRES_CAST) && type != Object.class) {
        var ctx = EnsoContext.get(this);
        if (this.ensoType == null) {
          CompilerDirectives.transferToInterpreterAndInvalidate();
          var builtin = ctx.getBuiltins().getByRepresentationType(type);
          if (builtin == null) {
            this.ensoType = ctx.getBuiltins().any();
          } else {
            this.ensoType = builtin.getType();
          }
        }
        var conv = executeConversion(value);
        if (conv == null) {
          CompilerDirectives.transferToInterpreter();
          var err = ctx.getBuiltins().error().makeTypeError(this.ensoType, value, type.getName());
          throw new PanicException(err, this);
        }
        return type.cast(conv);
      } else {
        return type.cast(value);
      }
    }

    public final Object processWarnings(VirtualFrame frame, Object result, ArgContext context) {
      assert context.warnings != null;
      if (this.appendWarningNode == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        this.appendWarningNode = insert(AppendWarningNode.build());
      }
      return appendWarningNode.executeAppend(frame, result, context.warnings);
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
        boolean requiresCast,
        boolean checkErrors,
        boolean checkPanicSentinel,
        boolean checkWarnings) {
      byte flags = 0x00;
      if (isSelf) {
        flags |= IS_SELF;
      }
      if (requiresCast) {
        flags |= REQUIRES_CAST;
      }
      if (checkErrors) {
        flags |= CHECK_ERRORS;
      }
      if (checkPanicSentinel) {
        flags |= CHECK_PANIC_SENTINEL;
      }
      if (checkWarnings) {
        flags |= CHECK_WARNINGS;
      }
      return BuiltinRootNodeFactory.ArgNodeGen.create(flags);
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
