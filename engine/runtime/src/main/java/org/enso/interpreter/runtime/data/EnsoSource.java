package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

/** Wrapper for exposing sources to Enso. Delegates to original methods with no behavior changes. */
@ExportLibrary(InteropLibrary.class)
public final class EnsoSource extends EnsoObject {
  private static final String[] MEMBERS = {
    "getLanguage", //
    "getName", //
    "getPath", //
    "isInternal", //
    "getCharacters", //
    "getLength", //
    "getLineCount", //
  };
  private final Source source;

  public EnsoSource(Source source) {
    this.source = source;
  }

  /*
  public CharSequence getCharacters(int lineNumber) {
    return source.getCharacters(lineNumber);
  }
  */

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  @TruffleBoundary
  @ExportMessage
  Object readMember(String name, @CachedLibrary("this") InteropLibrary node) {
    return switch (name) {
      case "getLanguage" -> textOrNull(node, source.getLanguage());
      case "getName" -> textOrNull(node, source.getName());
      case "getPath" -> textOrNull(node, source.getPath());
      case "isInternal" -> source.isInternal();
      case "getCharacters" -> textOrNull(node, source.getCharacters().toString());
      case "getLength" -> source.getLength();
      case "getLineCount" -> source.getLineCount();
      default -> throw EnsoContext.get(node).raiseAssertionPanic(node, name, null);
    };
  }

  private static EnsoObject textOrNull(Node where, String text) {
    if (text != null) {
      return Text.create(text);
    } else {
      var ctx = EnsoContext.get(where);
      return ctx.getNothing();
    }
  }

  @ExportMessage
  @TruffleBoundary
  boolean isMemberReadable(String name) {
    for (var n : MEMBERS) {
      if (name.equals(n)) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) {
    return ArrayLikeHelpers.wrapStrings(MEMBERS);
  }

  @Override
  @TruffleBoundary
  @ExportMessage
  public Object toDisplayString(boolean allowSideEffects) {
    return "EnsoSource{" + (source != null ? source.toString() : "") + "}";
  }
}
