package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

/** Wrapper for exposing sources to Enso. Delegates to original methods with no behavior changes. */
@ExportLibrary(InteropLibrary.class)
public final class EnsoSource implements EnsoObject {
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
      case "getLanguage" -> Text.create(source.getLanguage());
      case "getName" -> Text.create(source.getName());
      case "getPath" -> Text.create(source.getPath());
      case "isInternal" -> source.isInternal();
      case "getCharacters" -> Text.create(source.getCharacters().toString());
      case "getLength" -> source.getLength();
      case "getLineCount" -> source.getLineCount();
      default -> throw EnsoContext.get(node).raiseAssertionPanic(node, name, null);
    };
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
}
