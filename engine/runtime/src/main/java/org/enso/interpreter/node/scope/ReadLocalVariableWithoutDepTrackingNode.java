package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.FrameSlotTypeException;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.compiler.pass.analyse.FramePointer;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.execution.Ref;
import org.enso.polyglot.RuntimeID;

/**
 * Reads from a local target (variable or call target). Compared to {@link ReadLocalVariableNode},
 * the node will not participate in runtime dependency tracking setup by always unwrapping {@code
 * Ref} objects read from frames.
 *
 * <p>Note that local in this context does not necessarily mean that the variable is in the given
 * {@link Frame}. The {@code framePointer} field may point to the parent frame.
 */
@NodeInfo(shortName = "readVar", description = "Access local variable value.")
@NodeField(name = "name", type = String.class)
@NodeField(name = "framePointer", type = FramePointer.class)
public abstract class ReadLocalVariableWithoutDepTrackingNode extends ExpressionNode {
  private @CompilerDirectives.CompilationFinal RuntimeID id = null;

  abstract FramePointer getFramePointer();

  public abstract String getName();

  ReadLocalVariableWithoutDepTrackingNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param name the name of variable to read
   * @param pointer the pointer to the local target
   * @return a node that reads from {@code pointer}
   */
  public static ReadLocalVariableWithoutDepTrackingNode build(String name, FramePointer pointer) {
    return ReadLocalVariableWithoutDepTrackingNodeGen.create(name, pointer);
  }

  /**
   * Reads a {@code long} value from the frame.
   *
   * @param frame the stack frame to read from
   * @return the value read from the appropriate slot in {@code frame}
   * @throws FrameSlotTypeException when the specified frame slot does not contain a value with the
   *     expected type
   */
  @Specialization(rewriteOn = FrameSlotTypeException.class)
  protected long readLong(VirtualFrame frame) throws FrameSlotTypeException {
    if (getFramePointer().parentLevel() == 0)
      return frame.getLong(getFramePointer().frameSlotIdx());
    MaterializedFrame currentFrame = getProperFrame(frame);
    return currentFrame.getLong(getFramePointer().frameSlotIdx());
  }

  /**
   * Reads a generic value from the frame.
   *
   * @param frame the stack frame to read from
   * @return the value read from the appropriate slot in {@code frame}
   * @throws FrameSlotTypeException when the specified frame slot does not contain a value with the
   *     expected type
   */
  @Specialization(rewriteOn = FrameSlotTypeException.class)
  protected Object readGeneric(VirtualFrame frame) throws FrameSlotTypeException {
    if (getFramePointer().parentLevel() == 0)
      return ensureUnwrapped(frame.getObject(getFramePointer().frameSlotIdx()));
    MaterializedFrame currentFrame = getProperFrame(frame);
    return ensureUnwrapped(currentFrame.getObject(getFramePointer().frameSlotIdx()));
  }

  @Specialization
  protected Object readGenericValue(VirtualFrame frame) {
    if (getFramePointer().parentLevel() == 0)
      return ensureUnwrapped(frame.getValue(getFramePointer().frameSlotIdx()));
    MaterializedFrame currentFrame = getProperFrame(frame);
    return ensureUnwrapped(currentFrame.getValue(getFramePointer().frameSlotIdx()));
  }

  private Object ensureUnwrapped(Object o) {
    return o instanceof Ref r ? r.get() : o;
  }

  /**
   * Obtains the direct parent frame for a given frame.
   *
   * @param frame the frame whose parent needs to be found
   * @return the parent frame of {@code frame}
   */
  public MaterializedFrame getParentFrame(Frame frame) {
    return Function.ArgumentsHelper.getLocalScope(frame.getArguments());
  }

  /**
   * Gets the Enso parent frame for a given frame.
   *
   * <p>This method is responsible for getting the guest language parent frame for the current frame
   * by walking up the stack based on the scope in which the function was defined.
   *
   * @param frame the frame to find the Enso parent frame for
   * @return the guest language parent frame of {@code frame}
   */
  @ExplodeLoop
  public MaterializedFrame getProperFrame(Frame frame) {
    MaterializedFrame currentFrame = getParentFrame(frame);
    for (int i = 1; i < getFramePointer().parentLevel(); i++) {
      currentFrame = getParentFrame(currentFrame);
    }
    return currentFrame;
  }

  @Override
  public RuntimeID getId() {
    return this.id;
  }

  @Override
  public void setId(RuntimeID id) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.id = id;
  }

  @Override
  public Object getNodeObject() {
    return new VariableNodeObject(StandardTags.ReadVariableTag.NAME, getName());
  }

  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    if (super.hasTag(tag)) {
      return true;
    } else {
      return getSourceSectionBounds() != null && StandardTags.ReadVariableTag.class == tag;
    }
  }
}
