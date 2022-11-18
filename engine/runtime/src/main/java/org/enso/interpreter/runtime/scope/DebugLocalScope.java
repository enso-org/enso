package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * This class serves as a basic support for debugging with Chrome inspector.
 * Currently, only function scopes are supported.
 * Optimally, we should also implement ModuleScopes, and evaluation of an arbitrary expression.
 */
@ExportLibrary(InteropLibrary.class)
public class DebugLocalScope implements TruffleObject {
  // TODO: Implement ModuleScope as top level scope
  private final EnsoRootNode rootNode;
  // All bindings, including parent scopes
  private final Map<String, FramePointer> allBindings;
  private final List<List<String>> bindingsByLevels;
  private final int bindingsByLevelsIdx;
  private final MaterializedFrame frame;

  private DebugLocalScope(EnsoRootNode rootNode, MaterializedFrame frame, List<List<String>> bindingsByLevels, int bindingsByLevelsIdx) {
    this.rootNode = rootNode;
    this.frame = frame;
    this.allBindings = rootNode.getLocalScope().flattenBindings();
    this.bindingsByLevels =
        bindingsByLevels != null ? bindingsByLevels : gatherBindingsByLevels(allBindings);
    this.bindingsByLevelsIdx = bindingsByLevelsIdx;
    assert !this.bindingsByLevels.isEmpty();
    assert 0 <= this.bindingsByLevelsIdx && this.bindingsByLevelsIdx < this.bindingsByLevels.size();
  }

  private void log(String msg) {
    System.out.printf("DebugLocalScope@%s : %s\n",
        Integer.toHexString(hashCode()),
        msg);
  }

  public static DebugLocalScope createFromFrame(EnsoRootNode rootNode, MaterializedFrame frame) {
    var scope = new DebugLocalScope(rootNode, frame, null, 0);
    System.out.printf("new DebugLocalScope@%s : %s\n",
        Integer.toHexString(scope.hashCode()),
        scope);
    return scope;
  }

  private static DebugLocalScope createParent(DebugLocalScope childScope) {
    var scope = new DebugLocalScope(
        childScope.rootNode,
        childScope.frame,
        childScope.bindingsByLevels,
        childScope.bindingsByLevelsIdx + 1);
    System.out.printf("new DebugLocalScope@%s : %s\n",
        Integer.toHexString(scope.hashCode()),
        scope);
    return scope;
  }

  private static List<List<String>> gatherBindingsByLevels(Map<String, FramePointer> bindings) {
    int maxParentLevel = -1;
    for (var framePointer : bindings.values()) {
      if (framePointer.getParentLevel() > maxParentLevel) {
        maxParentLevel = framePointer.getParentLevel();
      }
    }
    assert maxParentLevel >= 0;

    // Get all binding names for a particular parent level
    List<List<String>> bindingsByLevels = new ArrayList<>(maxParentLevel + 1);
    for (int level = 0; level < maxParentLevel + 1; level++) {
      int finalLevel = level;
      List<String> levelBindings = bindings
          .entrySet()
          .stream()
          .filter(entry -> entry.getValue().getParentLevel() == finalLevel)
          .map(Entry::getKey)
          .collect(Collectors.toList());
      bindingsByLevels.add(levelBindings);
    }
    return bindingsByLevels;
  }

  @ExportMessage
  boolean hasLanguage() {
    return true;
  }

  @ExportMessage
  Class<? extends TruffleLanguage<?>> getLanguage() {
    return Language.class;
  }

  @ExportMessage
  boolean isScope() {
    return true;
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /**
   * Returns the members from the current local scope and all the parent scopes.
   * @param includeInternal
   * @return
   */
  @ExportMessage
  ScopeMembers getMembers(boolean includeInternal) {
    List<String> members = new ArrayList<>();
    for (int i = bindingsByLevelsIdx; i < bindingsByLevels.size(); i++) {
      members.addAll(bindingsByLevels.get(i));
    }
    return new ScopeMembers(members);
  }

  @ExportMessage
  boolean isMemberModifiable(String memberName) {
    // TODO
    return false;
  }

  @ExportMessage
  boolean isMemberInsertable(String memberName) {
    return false;
  }

  @ExportMessage
  boolean isMemberInvocable(String memberName) {
    // TODO
    return false;
  }

  @ExportMessage
  boolean hasMemberReadSideEffects(String member) {
    return false;
  }

  @ExportMessage
  boolean hasMemberWriteSideEffects(String member) {
    // TODO
    return false;
  }

  @ExportMessage
  boolean isMemberReadable(String memberName) {
    return allBindings.containsKey(memberName) &&
        getValue(frame, allBindings.get(memberName)) != null;
  }

  @ExportMessage
  Object readMember(String member) {
    FramePointer framePtr = allBindings.get(member);
    return getValue(frame, framePtr);
  }

  @ExportMessage
  void writeMember(String member, Object value) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  Object invokeMember(String member, Object[] args) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean hasScopeParent() {
    return bindingsByLevelsIdx < bindingsByLevels.size() - 1;
  }

  /**
   * ModuleScopes are not supported yet.
   * @return Parent scope (outer method).
   * @throws UnsupportedMessageException if there is no parent scope.
   */
  @ExportMessage
  Object getScopeParent() throws UnsupportedMessageException {
    if (!hasScopeParent()) {
      throw UnsupportedMessageException.create();
    } else {
      return createParent(this);
    }
  }

  @ExportMessage
  boolean hasSourceLocation() {
    return true;
  }

  @ExportMessage
  SourceSection getSourceLocation() {
    return rootNode.getSourceSection();
  }

  @ExportMessage
  @TruffleBoundary
  String toDisplayString(boolean allowSideEffects) {
    return rootNode.toString();
  }

  @Override
  public String toString() {
    return String.format("DebugLocalScope{rootNode = '%s', bindingsByLevels = %s, idx = %d}",
        rootNode.toString(), bindingsByLevels.toString(), bindingsByLevelsIdx);
  }

  private Object getValue(MaterializedFrame frame, FramePointer ptr) {
    return getProperFrame(frame, ptr).getValue(ptr.getFrameSlotIdx());
  }

  private MaterializedFrame getProperFrame(MaterializedFrame frame, FramePointer ptr) {
    MaterializedFrame currentFrame = frame;
    for (int i = 0; i < ptr.getParentLevel(); i++) {
      currentFrame = Function.ArgumentsHelper.getLocalScope(currentFrame.getArguments());
    }
    return currentFrame;
  }

  @ExportLibrary(InteropLibrary.class)
  final static class ScopeMembers implements TruffleObject {
    private final List<String> memberNames;

    ScopeMembers(List<String> memberNames) {
      this.memberNames = memberNames;
    }

    @ExportMessage
    boolean hasArrayElements() {
      return true;
    }

    @ExportMessage
    long getArraySize() {
      return memberNames.size();
    }

    @ExportMessage
    boolean isArrayElementReadable(long index) {
      return 0 <= index && index < memberNames.size();
    }

    @ExportMessage
    String readArrayElement(long index) {
      return memberNames.get((int) index);
    }

    @Override
    public String toString() {
      return memberNames.toString();
    }
  }
}
