package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "get_source_location",
    description = "Returns a textual representation of the location of the callsite.")
public class GetSourceLocationNode extends Node {

  Text execute(Object _this, long frames_to_skip) {
    var location = renderSourceLocation(frames_to_skip);
    return Text.create(location);
  }

  private static final String UNKNOWN = "Unknown Location";

  @TruffleBoundary
  private String renderSourceLocation(long framesToSkip) {
    long ix = framesToSkip + 1;
    if (ix > Integer.MAX_VALUE) throw new IllegalArgumentException("Too large offset.");
    var throwable = new PanicException(Text.create("Nothing"), getRootNode());
    throwable.fillInStackTrace();
    try {
      var truffleStack = TruffleStackTrace.getStackTrace(throwable);
      if (ix >= truffleStack.size()) return UNKNOWN;
      var frame = truffleStack.get((int) ix);
      var location = frame.getLocation();
      var sourceLoc = location.getEncapsulatingSourceSection();
      var path = sourceLoc.getSource().getPath();
      var ident = (path != null) ? path : sourceLoc.getSource().getName();
      var loc =
          (sourceLoc.getStartLine() == sourceLoc.getEndLine())
              ? (sourceLoc.getStartLine()
              + ":"
              + sourceLoc.getStartColumn()
              + "-"
              + sourceLoc.getEndColumn())
              : (sourceLoc.getStartLine() + "-" + sourceLoc.getEndLine());
      return ident + ":" + loc;
    } catch (NullPointerException e) {
      return UNKNOWN;
    }
  }
}
