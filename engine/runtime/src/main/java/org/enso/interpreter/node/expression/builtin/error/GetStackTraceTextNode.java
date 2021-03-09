package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.TruffleStackTraceElement;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.util.ArrayList;
import java.util.Collections;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Error",
    name = "get_stack_trace_text",
    description = "Returns a textual representation of the stack trace attached to an error.")
public class GetStackTraceTextNode extends Node {

  Text execute(DataflowError _this) {
    String repr = printStackTrace(_this);
    return Text.create(repr);
  }

  @TruffleBoundary
  String printStackTrace(Throwable throwable) {
    var sb = new StringBuilder();
    var truffleStack = TruffleStackTrace.getStackTrace(throwable);
    if (truffleStack == null) {
      sb.append("The error has no stacktrace attached to it.\n");
    } else {
      var fullStack = new ArrayList<>(truffleStack);
      var dropInitJava = new ArrayList<TruffleStackTraceElement>();
      boolean isInit = true;
      for (int i = fullStack.size() - 1; i >= 0; i--) {
        var elem = fullStack.get(i);
        if (isInit) {
          if (elem.getLocation().getRootNode().getLanguageInfo() != null) {
            isInit = false;
          }
        }

        if (!isInit) {
          dropInitJava.add(elem);
        }
      }
      Collections.reverse(dropInitJava);
      var stack = dropInitJava;
      if (dropInitJava.isEmpty()) {
        stack = fullStack;
      }

      boolean first = true;
      for (var errorFrame : stack) {
        if (errorFrame.getLocation() == null) {
          continue;
        }
        var rootNode = errorFrame.getLocation().getRootNode();
        if (rootNode == null) {
          continue;
        }
        var languageInfo = rootNode.getLanguageInfo();
        var langId = (languageInfo == null) ? "java" : languageInfo.getId();
        var fName = rootNode.getName();
        var src = "Internal";
        var sourceLoc = errorFrame.getLocation().getEncapsulatingSourceSection();
        if (sourceLoc != null) {
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
          src = ident + ":" + loc;
        }
        if (first) {
          first = false;
        } else {
          sb.append('\n');
        }
        sb.append("        at <" + langId + "> " + fName + "(" + src + ")");
      }
    }
    return sb.toString();
  }
}
