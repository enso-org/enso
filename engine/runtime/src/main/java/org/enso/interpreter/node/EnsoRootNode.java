package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;

/** A common base class for all kinds of root node in Enso. */
public abstract class EnsoRootNode extends RootNode {
  private final String name;
  private final SourceSection sourceSection;
  private final FrameSlot stateFrameSlot;
  private @CompilerDirectives.CompilationFinal TruffleLanguage.ContextReference<Context>
      contextReference;
  private @CompilerDirectives.CompilationFinal TruffleLanguage.LanguageReference<Language>
      languageReference;

  /**
   * Constructs the root node.
   *
   * @param language the language instance in which this will execute
   * @param frameDescriptor a reference to the construct root frame
   * @param name the name of the construct
   * @param sourceSection a reference to the source code being executed
   * @param stateFrameSlot the code to compile and execute
   */
  public EnsoRootNode(
      Language language,
      FrameDescriptor frameDescriptor,
      String name,
      SourceSection sourceSection,
      FrameSlot stateFrameSlot) {
    super(language, frameDescriptor);
    this.name = name;
    this.sourceSection = sourceSection;
    this.stateFrameSlot = stateFrameSlot;
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
}
