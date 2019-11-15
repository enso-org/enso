package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A common base class for all kinds of root node in Enso. */
public abstract class EnsoRootNode extends RootNode {
  private final String name;
  private final SourceSection sourceSection;
  private final FrameSlot stateFrameSlot;
  private final LocalScope localScope;
  private final ModuleScope moduleScope;
  private @CompilerDirectives.CompilationFinal TruffleLanguage.ContextReference<Context>
      contextReference;
  private @CompilerDirectives.CompilationFinal TruffleLanguage.LanguageReference<Language>
      languageReference;

  /**
   * Constructs the root node.
   *
   * @param language the language instance in which this will execute
   * @param localScope a reference to the construct local scope
   * @param moduleScope a reference to the construct module scope
   * @param name the name of the construct
   * @param sourceSection a reference to the source code being executed
   */
  public EnsoRootNode(
      Language language,
      LocalScope localScope,
      ModuleScope moduleScope,
      String name,
      SourceSection sourceSection) {
    super(language, localScope.getFrameDescriptor());
    this.name = name;
    this.localScope = localScope;
    this.moduleScope = moduleScope;
    this.sourceSection = sourceSection;
    this.stateFrameSlot =
        localScope.getFrameDescriptor().findOrAddFrameSlot("<<state>>", FrameSlotKind.Object);
  }

  /**
   * Gets a reference to the language context associated with this program.
   *
   * @return a reference to the language context
   */
  public Context getContext() {
    if (contextReference == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      contextReference = lookupContextReference(Language.class);
    }

    return contextReference.get();
  }

  /**
   * Creates a string representation of this node.
   *
   * @return a string representation of the node
   */
  @Override
  public String toString() {
    return this.name;
  }

  /**
   * Sets whether the node is tail-recursive.
   *
   * @param isTail whether or not the node is tail-recursive
   */
  public abstract void setTail(boolean isTail);

  /**
   * Gets the frame slot containing the program state.
   *
   * @return the state frame slot
   */
  public FrameSlot getStateFrameSlot() {
    return this.stateFrameSlot;
  }

  /**
   * Gets the source code represented by this node.
   *
   * @return a reference to the source code
   */
  @Override
  public SourceSection getSourceSection() {
    return sourceSection;
  }

  /**
   * Gets the local scope this node expects to work with
   *
   * @return the local scope for this node
   */
  public LocalScope getLocalScope() {
    return localScope;
  }

  /**
   * Gets the module scope this node was defined with
   *
   * @return the module scope for this node
   */
  public ModuleScope getModuleScope() {
    return moduleScope;
  }
}
