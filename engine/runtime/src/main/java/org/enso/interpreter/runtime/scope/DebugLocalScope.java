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
import java.util.Comparator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * This class serves as a basic support for debugging with Chrome inspector. Currently, only
 * function scopes are supported.
 *
 * <p>Some of the features that remain to be implemented are:
 *
 * <ul>
 *   <li>Module scopes. How to display imports in chrome devtools? Get inspiration from Python?
 *   <li>Evaluation of an arbitrary expression
 * </ul>
 */
@ExportLibrary(InteropLibrary.class)
public class DebugLocalScope implements TruffleObject {
  private final EnsoRootNode rootNode;

  /** All the bindings, including the parent scopes. */
  private final Map<String, FramePointer> allBindings;

  /**
   * The inner lists represent particular scope in a scope hierarchy. For example, for the following
   * snippet:
   *
   * <pre>
   * func =
   *     x = 1
   *     inner_func =
   *         y = 2
   *         y
   *     inner_func
   * </pre>
   *
   * the value of this field (for `inner_func` scope) would be {@code [['x'], ['y']]}
   */
  private final List<List<String>> bindingsByLevels;

  /** Index of the current scope into {@link #bindingsByLevels} list. */
  private final int bindingsByLevelsIdx;

  private final MaterializedFrame frame;

  private DebugLocalScope(
      EnsoRootNode rootNode,
      MaterializedFrame frame,
      List<List<String>> bindingsByLevels,
      int bindingsByLevelsIdx) {
    assert bindingsByLevels != null;
    this.rootNode = rootNode;
    this.frame = frame;
    this.allBindings = rootNode.getLocalScope().flattenBindings();
    this.bindingsByLevels = bindingsByLevels;
    this.bindingsByLevelsIdx = bindingsByLevelsIdx;
    assert !this.bindingsByLevels.isEmpty();
    assert 0 <= this.bindingsByLevelsIdx && this.bindingsByLevelsIdx < this.bindingsByLevels.size();
  }

  public static DebugLocalScope createFromFrame(EnsoRootNode rootNode, MaterializedFrame frame) {
    return new DebugLocalScope(
        rootNode, frame, gatherBindingsByLevels(rootNode.getLocalScope().flattenBindings()), 0);
  }

  private static DebugLocalScope createParent(DebugLocalScope childScope) {
    return new DebugLocalScope(
        childScope.rootNode,
        childScope.frame,
        childScope.bindingsByLevels,
        childScope.bindingsByLevelsIdx + 1);
  }

  private static List<List<String>> gatherBindingsByLevels(Map<String, FramePointer> bindings) {
    int maxParentLevel =
        bindings.values().stream()
            .max(Comparator.comparingInt(FramePointer::getParentLevel))
            .orElseThrow()
            .getParentLevel();

    // Get all binding names for a particular parent level
    List<List<String>> bindingsByLevels = new ArrayList<>(maxParentLevel + 1);
    for (int level = 0; level < maxParentLevel + 1; level++) {
      final int finalLevel = level;
      List<String> levelBindings =
          bindings.entrySet().stream()
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
    return EnsoLanguage.class;
  }

  @ExportMessage
  boolean isScope() {
    return true;
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /** Returns the members from the current local scope and all the parent scopes. */
  @ExportMessage
  ScopeMembers getMembers(boolean includeInternal) {
    List<String> members = new ArrayList<>();
    bindingsByLevels.stream().skip(bindingsByLevelsIdx).forEach(members::addAll);
    return new ScopeMembers(members);
  }

  @ExportMessage
  boolean isMemberModifiable(String memberName) {
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
    return false;
  }

  @ExportMessage
  boolean isMemberReadable(String memberName) {
    // When a value in a frame is null, it means that the corresponding
    // AssignmentNode was not run yet, and the slot kind of the
    // FrameDescriptor would be Illegal.
    return allBindings.containsKey(memberName)
        && getValue(frame, allBindings.get(memberName)) != null;
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
   * Returns the parent scope. ModuleScopes are not supported yet.
   *
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
    return String.format(
        "DebugLocalScope{rootNode = '%s', bindingsByLevels = %s, idx = %d}",
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

  /** Simple interop wrapper for a list of strings. */
  @ExportLibrary(InteropLibrary.class)
  static final class ScopeMembers implements TruffleObject {
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
