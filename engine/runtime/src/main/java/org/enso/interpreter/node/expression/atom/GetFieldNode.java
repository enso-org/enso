package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "get_field", description = "A base for auto-generated Atom getters.")
public class GetFieldNode extends RootNode {
  private final int index;
  private final String name;
  private final Type type;

  /**
   * Creates a new instance of this node.
   *
   * @param language the current language instance.
   * @param index the index this node should use for field lookup.
   */
  public GetFieldNode(TruffleLanguage<?> language, int index, Type type, String name) {
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
  public Stateful execute(VirtualFrame frame) {
    if (!(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0] instanceof Atom atom)) {
      var msg = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
      throw new PanicException(
          Context.get(this)
              .getBuiltins()
              .error()
              .makeInexhaustivePatternMatchError(msg),
          this);
    }
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, atom.getFields()[index]);
  }

  @Override
  public String getQualifiedName() {
    return type.getQualifiedName().createChild(name).toString();
  }

  @Override
  public String getName() {
    return type.getName() + "." + name;
  }
}
