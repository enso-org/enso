package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.TruffleStackTraceElement;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.callable.IndirectInvokeMethodNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.expression.builtin.error.CatchPanicNode;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** An exception type for user thrown panic exceptions. */
@ExportLibrary(value = InteropLibrary.class, delegateTo = "payload")
@ExportLibrary(TypesLibrary.class)
public final class PanicException extends AbstractTruffleException {
  private EnsoContext ctx;
  final Object payload;
  private String cacheMessage;
  private EnsoObject stackTrace;
  private Node caughtBy;

  /**
   * Creates new user visible panic.
   *
   * @param payload arbitrary, user-provided payload carried by this exception
   * @param location the node throwing this exception, for use in guest stack traces
   */
  public PanicException(Object payload, Node location) {
    this(EnsoContext.get(location), payload, null, location);
  }

  /**
   * Creates user visible panic with additional cause.
   *
   * @param ctx context the exception is associated with
   * @param payload arbitrary, user-provided payload carried by this exception
   * @param cause additional exception to carry information about the panic
   * @param location the node throwing this exception, for use in guest stack traces
   */
  public PanicException(EnsoContext ctx, Object payload, Throwable cause, Node location) {
    super(null, cause, UNLIMITED_STACK_TRACE, location);
    assert InteropLibrary.isValidValue(payload) : "Only interop values are supported: " + payload;
    this.payload = payload;
    this.ctx = ctx;
  }

  /** package private for use from {@link DataflowError#rethrow}. */
  PanicException(DataflowError err) {
    super(err);
    this.payload = err.getPayload();
    this.ctx = err.ctx();
  }

  /**
   * Returns the payload in the panic.
   *
   * @return the panic payload
   */
  public Object getPayload() {
    return payload;
  }

  @Override
  public String getMessage() {
    if (cacheMessage == null) {
      return computeMessage();
    }
    return cacheMessage;
  }

  /**
   * Obtains associated context, if any.
   *
   * @return associated context or {@code null}
   */
  final EnsoContext ctx() {
    return ctx;
  }

  @CompilerDirectives.TruffleBoundary
  private String computeMessage() {
    String msg;
    var library = InteropLibrary.getUncached();
    try {
      var info = library.getExceptionMessage(this);
      msg = library.asString(info);
    } catch (IllegalStateException
        | StackOverflowError
        | AssertionError
        | UnsupportedMessageException e) {
      var l = logger();
      l.atError().log("Cannot compute message for " + payload, e);
      l.error("Exception location: " + getLocation());
      if (getLocation() != null) {
        l.error("  location source: " + getLocation().getEncapsulatingSourceSection());
        l.error("  location class: " + getLocation().getClass().getName());
        l.error("  location string: " + getLocation());
      }
      l.error("  payload class: " + payload.getClass().getName());
      if (payload instanceof Atom atom) {
        l.error("  payload cons: " + atom.getConstructor());
        l.error("  payload type: " + atom.getConstructor().getType());
      }
      msg = TypeToDisplayTextNode.getUncached().execute(payload);
    }
    cacheMessage = msg;
    return msg;
  }

  @Override
  public String toString() {
    return getMessage();
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  RuntimeException throwException() {
    throw this;
  }

  @ExportMessage
  boolean hasExceptionMessage() {
    return true;
  }

  @NeverDefault
  static UnresolvedSymbol toDisplayText(EnsoContext ctx, IndirectInvokeMethodNode payloads)
      throws UnsupportedMessageException {
    var scope = ctx.getBuiltins().panic().getDefinitionScope();
    return UnresolvedSymbol.build(Constants.Names.TO_DISPLAY_TEXT, scope);
  }

  @ExportMessage
  Object getExceptionMessage(
      @Cached IndirectInvokeMethodNode payloads,
      @Cached(value = "toDisplayText(this.ctx(), payloads)", allowUncached = true)
          UnresolvedSymbol toDisplayText,
      @CachedLibrary(limit = "3") InteropLibrary strings,
      @Cached TypeToDisplayTextNode typeToDisplayTextNode) {
    return handleExceptionMessage(
        payload, ctx(), payloads, toDisplayText, strings, typeToDisplayTextNode);
  }

  @CompilerDirectives.TruffleBoundary
  static Object handleExceptionMessage(
      Object payload,
      EnsoContext ctx,
      IndirectInvokeMethodNode payloads,
      UnresolvedSymbol toDisplayText,
      InteropLibrary strings,
      TypeToDisplayTextNode typeToDisplayTextNode) {
    Supplier<Object> action =
        () ->
            findExceptionMessage(
                payload, ctx, payloads, toDisplayText, strings, typeToDisplayTextNode);
    if (ctx != null) {
      return ctx.withinCtx(payloads, action);
    } else {
      return action.get();
    }
  }

  private static Object findExceptionMessage(
      Object payload,
      EnsoContext ctx,
      IndirectInvokeMethodNode payloads,
      UnresolvedSymbol toDisplayText,
      InteropLibrary strings,
      TypeToDisplayTextNode typeToDisplayTextNode) {
    var text =
        payloads.execute(
            null,
            State.create(ctx),
            toDisplayText,
            payload,
            new Object[] {payload},
            new CallArgumentInfo[] {new CallArgumentInfo("self")},
            DefaultsExecutionMode.EXECUTE,
            ArgumentsExecutionMode.EXECUTE,
            TailStatus.NOT_TAIL,
            0);
    try {
      return Text.create(strings.asString(text));
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      logger().error("Cannot convert " + text + " to string", e);
      return Text.create(typeToDisplayTextNode.execute(payload));
    }
  }

  @ExportMessage
  Type getType(@Bind Node node) {
    return EnsoContext.get(node).getBuiltins().panic();
  }

  @ExportMessage
  Type getMetaObject(@Bind Node node) {
    return EnsoContext.get(node).getBuiltins().panic();
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  ExceptionType getExceptionType() {
    return ExceptionType.RUNTIME_ERROR;
  }

  @ExportMessage
  int getExceptionExitStatus() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean hasExceptionStackTrace() {
    return true;
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final Object getExceptionStackTrace(@Bind Node queryNode) {
    if (stackTrace == null) {
      if (ctx != null) {
        stackTrace = ctx.withinCtx(queryNode, () -> computeStackTrace(queryNode));
      } else {
        stackTrace = computeStackTrace(queryNode);
      }
    }
    return stackTrace;
  }

  @CompilerDirectives.TruffleBoundary
  private EnsoObject computeStackTrace(Node queryNode) {
    var rawStack = TruffleStackTrace.getStackTrace(this);
    var caughtIndex = findNodeIndexInStack(caughtBy, rawStack);
    if (caughtIndex >= 0) {
      var nowEx = new PanicException(0L, queryNode);
      var nowStack = TruffleStackTrace.getStackTrace(nowEx);
      var queryIndex = rawStack.size() - nowStack.size();
      if (queryIndex >= 0 && caughtIndex > queryIndex) {
        // skip the part of stack between queryNode and caughtBy
        // See https://github.com/enso-org/enso/pull/12024#discussion_r1909850986 for deeper
        // explanation.
        var arr = new ArrayList<Object>();
        for (var frame : rawStack.subList(0, queryIndex)) {
          arr.add(frame.getGuestObject());
        }
        for (var frame : rawStack.subList(caughtIndex, rawStack.size())) {
          arr.add(frame.getGuestObject());
        }
        return ArrayLikeHelpers.asVectorWithCheckAt(arr.toArray());
      }
    }
    var fullArr = new ArrayList<Object>();
    for (var frame : rawStack) {
      fullArr.add(frame.getGuestObject());
    }
    return ArrayLikeHelpers.asVectorWithCheckAt(fullArr.toArray());
  }

  private static int findNodeIndexInStack(Node n, List<TruffleStackTraceElement> stack) {
    if (n == null) {
      return -1;
    }
    var rn = n.getRootNode();
    for (int idx = 0; idx < stack.size(); idx++) {
      if (rn == stack.get(idx).getTarget().getRootNode()) {
        return idx;
      }
    }
    return -1;
  }

  @ExportMessage
  boolean isExceptionIncompleteSource() {
    return false;
  }

  @ExportMessage
  boolean hasSourceLocation() {
    var location = getLocation();
    return location != null && location.getEncapsulatingSourceSection() != null;
  }

  @ExportMessage(name = "getSourceLocation")
  SourceSection getSourceSection() throws UnsupportedMessageException {
    SourceSection section = getLocation().getEncapsulatingSourceSection();
    if (section == null) {
      throw UnsupportedMessageException.create();
    } else {
      return getLocation().getEncapsulatingSourceSection();
    }
  }

  private static Logger logger() {
    CompilerDirectives.transferToInterpreter();
    return LoggerFactory.getLogger(PanicException.class);
  }

  /**
   * Whoever catches a {@code PanicException} should associate its location with it, so proper stack
   * traces can be computed later. Because the proper stacktrace information is relative to the last
   * caught location.
   *
   * @param node the node who caught the panic the last time
   */
  public final void assignCaughtLocation(CatchPanicNode node) {
    this.caughtBy = node;
  }
}
