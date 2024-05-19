package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

/**
 * Wrapper for exposing source sections in Enso. Delegates to the original methods with no behaviour
 * changes.
 */
@ExportLibrary(InteropLibrary.class)
public final class EnsoSourceSection implements EnsoObject {
  private static final String[] MEMBERS = {
    "getStartLine", //
    "getEndLine", //
    "getEndColumn", //
    "getCharIndex", //
    "getCharLength", //
    "getCharEndIndex", //
    "getCharacters", //
    "getStartColumn", //
    "isAvailable", //
    "hasLines", //
    "hasColumns", //
    "hasCharIndex", //
    "getSource", //
  };
  private final SourceSection sourceSection;

  public EnsoSourceSection(SourceSection sourceSection) {
    this.sourceSection = sourceSection;
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  @TruffleBoundary
  @ExportMessage
  Object readMember(String name, @CachedLibrary("this") InteropLibrary node) {
    return switch (name) {
      case "getStartLine" -> sourceSection.getStartLine();
      case "getEndLine" -> sourceSection.getEndLine();
      case "getEndColumn" -> sourceSection.getEndColumn();
      case "getCharIndex" -> sourceSection.getCharIndex();
      case "getCharLength" -> sourceSection.getCharLength();
      case "getCharEndIndex" -> sourceSection.getCharEndIndex();
      case "getCharacters" -> Text.create(sourceSection.getCharacters().toString());
      case "getStartColumn" -> sourceSection.getStartColumn();
      case "isAvailable" -> sourceSection.isAvailable();
      case "hasLines" -> sourceSection.hasLines();
      case "hasColumns" -> sourceSection.hasColumns();
      case "hasCharIndex" -> sourceSection.hasCharIndex();
      case "getSource" -> new EnsoSource(sourceSection.getSource());
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
