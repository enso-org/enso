package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;

@NodeInfo(shortName = "get_field", description = "A base for auto-generated Atom getters.")
final class GetFieldNode extends RootNode {
  private final int index;
  private final String name;
  private final Type type;

  private @Child StructsLibrary structs = StructsLibrary.getFactory().createDispatched(10);

  /**
   * Creates a new instance of this node.
   *
   * @param language the current language instance.
   * @param index the index this node should use for field lookup.
   */
  GetFieldNode(TruffleLanguage<?> language, int index, Type type, String name) {
    super(language);
    this.index = index;
    this.type = type;
    this.name = name;
  }

  /**
   * Executes the node, by taking the first argument from the frame and plucking the proper field
   * from it.
   *
   * @param frame current execution frame
   * @return the field value at predefined index
   */
  public Object execute(VirtualFrame frame) {
    // this is safe, as only Atoms will ever get here through method dispatch.
    Atom atom = (Atom) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
    return structs.getField(atom, index);
  }

  @Override
  public String getQualifiedName() {
    return type.getQualifiedName().createChild(name).toString();
  }

  @Override
  public String getName() {
    return type.getName() + "." + name;
  }

  @Override
  public boolean isCloningAllowed() {
    return true;
  }

  @Override
  protected boolean isCloneUninitializedSupported() {
    return true;
  }

  @Override
  protected GetFieldNode cloneUninitialized() {
    return new GetFieldNode(getLanguage(EnsoLanguage.class), index, type, name);
  }
}
