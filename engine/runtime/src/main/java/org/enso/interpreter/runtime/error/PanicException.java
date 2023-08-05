package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
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
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.callable.IndirectInvokeMethodNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNodeGen;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

/** An exception type for user thrown panic exceptions. */
@ExportLibrary(value = InteropLibrary.class, delegateTo = "payload")
@ExportLibrary(TypesLibrary.class)
public final class PanicException extends AbstractTruffleException {
  final Object payload;
  private String cacheMessage;

  /**
   * Creates new user visible panic.
   *
   * @param payload arbitrary, user-provided payload carried by this exception
   * @param location the node throwing this exception, for use in guest stack traces
   */
  public PanicException(Object payload, Node location) {
    this(payload, null, location);
  }

  /**
   * Creates user visible panic with additional cause.
   *
   * @param payload arbitrary, user-provided payload carried by this exception
   * @param cause additional exception to carry information about the panic
   * @param location the node throwing this exception, for use in guest stack traces
   */
  public PanicException(Object payload, Throwable cause, Node location) {
    super(null, cause, UNLIMITED_STACK_TRACE, location);
    if (!InteropLibrary.isValidValue(payload)) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalArgumentException("Only interop values are supported: " + payload);
    }
    this.payload = payload;
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

  @CompilerDirectives.TruffleBoundary
  private String computeMessage() {
    String msg;
    InteropLibrary library = InteropLibrary.getUncached();
    try {
      msg = library.asString(library.getExceptionMessage(this));
    } catch (UnsupportedMessageException e) {
      msg = TypeToDisplayTextNodeGen.getUncached().execute(payload);
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
  static UnresolvedSymbol toDisplayText(IndirectInvokeMethodNode payloads) {
    var ctx = EnsoContext.get(payloads);
    var scope = ctx.getBuiltins().panic().getDefinitionScope();
    return UnresolvedSymbol.build("to_display_text", scope);
  }

  @ExportMessage
  Object getExceptionMessage(
      @Cached IndirectInvokeMethodNode payloads,
      @Cached(value = "toDisplayText(payloads)", allowUncached = true)
          UnresolvedSymbol toDisplayText,
      @CachedLibrary(limit = "3") InteropLibrary strings,
      @Cached TypeToDisplayTextNode typeToDisplayTextNode) {
    var ctx = EnsoContext.get(payloads);
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
      return Text.create(typeToDisplayTextNode.execute(payload));
    }
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().panic();
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().panic();
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
  int getExceptionExitStatus() {
    return 1;
  }

  @ExportMessage
  boolean isExceptionIncompleteSource() {
    return false;
  }

  @ExportMessage
  boolean hasSourceLocation() {
    return getLocation().getEncapsulatingSourceSection() != null;
  }

  @ExportMessage(name = "getSourceLocation")
  SourceSection getSourceSection() throws UnsupportedMessageException {
    SourceSection loc = getLocation().getEncapsulatingSourceSection();
    if (loc == null) {
      throw UnsupportedMessageException.create();
    }
    return getLocation().getEncapsulatingSourceSection();
  }
}
