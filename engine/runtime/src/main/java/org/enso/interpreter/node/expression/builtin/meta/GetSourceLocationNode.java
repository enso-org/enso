package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_source_location_builtin",
    description = "Returns a textual representation of the location of the callsite.")
public class GetSourceLocationNode extends Node {

  Text execute(Object self, long frames_to_skip) {
    var location = renderSourceLocation(frames_to_skip);
    return Text.create(location);
  }

  private static final String UNKNOWN = "Unknown Location";

  @TruffleBoundary
  private String renderSourceLocation(final long framesToSkip) {
    var visitor =
        new FrameInstanceVisitor<String>() {
          long offset = framesToSkip + 1;

          @Override
          public String visitFrame(FrameInstance frameInstance) {
            if (offset == 0) {
              var node = frameInstance.getCallNode();
              if (node == null) return UNKNOWN;
              var section = node.getEncapsulatingSourceSection();
              if (section == null) return UNKNOWN;
              var source = section.getSource();
              var path = source.getPath();
              var ident = (path != null) ? path : source.getName();
              var loc =
                  (section.getStartLine() == section.getEndLine())
                      ? (section.getStartLine()
                          + ":"
                          + section.getStartColumn()
                          + "-"
                          + section.getEndColumn())
                      : (section.getStartLine() + "-" + section.getEndLine());
              return ident + ":" + loc;
            } else {
              offset--;
              return null;
            }
          }
        };

    var result = Truffle.getRuntime().iterateFrames(visitor);
    return result == null ? UNKNOWN : result;
  }
}
