package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "get_field", description = "A base for auto-generated Atom getters.")
public class GetFieldWithMatchNode extends RootNode {
  public static class GetterPair {
    private final AtomConstructor target;
    private final int index;

    public GetterPair(AtomConstructor target, int index) {
      this.target = target;
      this.index = index;
    }

    public AtomConstructor getTarget() {
      return target;
    }

    public int getIndex() {
      return index;
    }
  }

  private final String name;
  private final Text nameText;
  private final Type type;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) GetterPair[] getterPairs;

  /**
   * Creates a new instance of this node.
   *
   * @param language the current language instance.
   * @param index the index this node should use for field lookup.
   */
  public GetFieldWithMatchNode(
      TruffleLanguage<?> language, String name, Type type, GetterPair[] getterPairs) {
    super(language);
    this.name = name;
    this.type = type;
    this.nameText = Text.create(name);
    this.getterPairs = getterPairs;
  }

  @ExplodeLoop
  public Stateful execute(VirtualFrame frame) {
    // this is safe, as only Atoms will ever get here through method dispatch.
    Atom atom = (Atom) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    var constructor = atom.getConstructor();
    for (int i = 0; i < getterPairs.length; i++) {
      var getter = getterPairs[i];
      if (getter.target == constructor) {
        return new Stateful(state, atom.getFields()[getter.index]);
      }
    }
    throw new PanicException(
        Context.get(this).getBuiltins().error().getNoSuchFieldError().newInstance(atom, nameText),
        this);
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
