package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import java.util.Arrays;

import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;

/**
 * Wrapper for host objects. Is a workaround for a bug in chromeinspector
 * (https://github.com/oracle/graal/issues/5513). All the host objects should
 * be wrapped before passing to instruments.
 * <p>
 * Note that specifying delegate in the {@code @ExportLibrary} annotation does
 * not work.
 */
@ExportLibrary(InteropLibrary.class)
public final class HostObjectDebugWrapper implements TruffleObject {

  private final String stringRepr;

  public HostObjectDebugWrapper(Object hostObject) {
    Env env = EnsoContext.get(null).getEnvironment();
    InteropLibrary interop = InteropLibrary.getUncached();
    assert env.isHostObject(hostObject);
    StringBuilder sb = new StringBuilder();
    sb.append("HostObject{");
    try {
      if (interop.hasMetaObject(hostObject)) {
        Object metaObject = interop.getMetaObject(hostObject);
        Object metaQualifiedName = interop.getMetaQualifiedName(metaObject);
        sb.append(interop.asString(metaQualifiedName)).append(": ");
      }
      sb.append("'").append(interop.asString(interop.toDisplayString(hostObject))).append("'");
    } catch (UnsupportedMessageException e) {
      sb.append("unknown");
    }
    sb.append("}");
    this.stringRepr = sb.toString();
  }

  /**
   * Wraps given object in {@link HostObjectDebugWrapper} if necessary. The returned
   * wrapper is a string from the Truffle perspective.
   * <p>
   * Serves as a workaround for https://github.com/oracle/graal/issues/5513.
   *
   * @param object Object to potentialy wrap in {@link HostObjectDebugWrapper}.
   */
  @TruffleBoundary
  public static Object wrapHostValues(Object object, InteropLibrary interop, StructsLibrary structs) {
    if (object instanceof Atom atom) {
      Object[] fields = structs.getFields(atom);
      Object[] wrappedFields = new Object[fields.length];
      for (int i = 0; i < fields.length; i++) {
        wrappedFields[i] = wrapHostValues(fields[i], interop, structs);
      }
      return atom.getConstructor().newInstance(wrappedFields);
    } else if (isHostValue(object, interop)) {
      return new HostObjectDebugWrapper(object);
    } else {
      return object;
    }
  }

  private static boolean isHostValue(Object value, InteropLibrary interop) {
    return EnsoContext.get(interop).getEnvironment().isHostObject(value);
  }


  @ExportMessage
  boolean hasLanguage() {
    return true;
  }

  @ExportMessage
  Class<? extends TruffleLanguage<?>> getLanguage() {
    return EnsoLanguage.class;
  }

  @ExportMessage
  boolean isString() {
    return true;
  }

  @ExportMessage
  String asString() {
    return stringRepr;
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return stringRepr;
  }
}
