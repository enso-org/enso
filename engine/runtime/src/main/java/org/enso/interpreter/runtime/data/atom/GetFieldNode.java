package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;

@NodeInfo(
    shortName = "get_field",
    description = "Returns a single field from an Atom based on the given index.")
final class GetFieldNode extends GetFieldBaseNode {
  private final int index;

  private @Child StructsLibrary structs = StructsLibrary.getFactory().createDispatched(10);

  /**
   * Creates a new instance of this node.
   *
   * @param language the current language instance.
   * @param index the index this node should use for field lookup.
   */
  GetFieldNode(EnsoLanguage language, int index, Type type, String name, ModuleScope moduleScope) {
    super(language, type, name, moduleScope);
    this.index = index;
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
  public boolean isCloningAllowed() {
    return true;
  }

  @Override
  protected boolean isCloneUninitializedSupported() {
    return true;
  }

  @Override
  protected GetFieldNode cloneUninitialized() {
    return new GetFieldNode(
        getLanguage(EnsoLanguage.class), index, type, fieldName, getModuleScope());
  }
}
