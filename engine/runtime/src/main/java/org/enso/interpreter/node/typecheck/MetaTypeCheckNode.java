package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import org.enso.interpreter.node.expression.builtin.meta.IsValueOfTypeNode;
import org.enso.interpreter.runtime.util.CachingSupplier;

/**
 * Node for checking {@code polyglot java import} types. This class (and its subclasses)
 * are an implementation detail. The API to perform the is in {@link TypeCheckValueNode}.
 */
non-sealed abstract class MetaTypeCheckNode extends AbstractTypeCheckNode {
  private final CachingSupplier<? extends Object> expectedSupplier;
  @CompilerDirectives.CompilationFinal private String expectedTypeMessage;

  MetaTypeCheckNode(String name, CachingSupplier<? extends Object> expectedMetaSupplier) {
    super(name);
    this.expectedSupplier = expectedMetaSupplier;
  }

  abstract Object executeCheckOrConversion(VirtualFrame frame, Object value);

  @Override
  Object findDirectMatch(VirtualFrame frame, Object value) {
    return executeCheckOrConversion(frame, value);
  }

  @Specialization
  Object verifyMetaObject(VirtualFrame frame, Object v, @Cached IsValueOfTypeNode isA) {
    if (isAllFitValue(v)) {
      return v;
    }
    if (isA.execute(expectedSupplier.get(), v, true)) {
      return v;
    } else {
      return null;
    }
  }

  @Override
  String expectedTypeMessage() {
    if (expectedTypeMessage != null) {
      return expectedTypeMessage;
    }
    CompilerDirectives.transferToInterpreterAndInvalidate();
    com.oracle.truffle.api.interop.InteropLibrary iop = InteropLibrary.getUncached();
    try {
      expectedTypeMessage = iop.asString(iop.getMetaQualifiedName(expectedSupplier.get()));
    } catch (UnsupportedMessageException ex) {
      expectedTypeMessage = expectedSupplier.get().toString();
    }
    return expectedTypeMessage;
  }
}
