package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.util.List;
import org.enso.interpreter.runtime.callable.atom.UnboxingAtom.FieldGetterNode;
import org.enso.interpreter.runtime.callable.atom.UnboxingAtom.FieldSetterNode;

/**
 * A version of {@link org.enso.interpreter.runtime.callable.atom.Atom} that stores its fields in an
 * array of objects. This will be slow most of the time, and is the fallback version of {@link
 * org.enso.interpreter.runtime.callable.atom.Atom}. For a better optimized version, see {@link
 * org.enso.interpreter.runtime.callable.atom.UnboxingAtom}.
 */
@ExportLibrary(StructsLibrary.class)
public final class BoxingAtom extends Atom {
  static final NodeFactory<? extends UnboxingAtom.InstantiatorNode> FACTORY =
      new InstantiatorFactory();

  @SuppressWarnings("unchecked")
  static NodeFactory<UnboxingAtom.FieldGetterNode>[] getFieldGetterNodeFactories(int arity) {
    var arr = new NodeFactory[arity];
    for (int idx = 0; idx < arr.length; idx++) {
      arr[idx] = new GetterFactory(idx);
    }
    return arr;
  }

  @SuppressWarnings("unchecked")
  static NodeFactory<UnboxingAtom.FieldSetterNode>[] getFieldSetterNodeFactories(int arity) {
    var arr = new NodeFactory[arity];
    for (int idx = 0; idx < arr.length; idx++) {
      arr[idx] = new SetterFactory(idx);
    }
    return arr;
  }

  private final Object[] fields;

  BoxingAtom(AtomConstructor constructor, Object... fields) {
    super(constructor);
    this.fields = fields;
  }

  @ExportMessage
  Object[] getFields() {
    return fields;
  }

  @ExportMessage
  static class GetField {
    @Specialization(
        guards = {"cachedLayout == atom.getConstructor().getBoxedLayout()", "cachedIndex == index"},
        limit = "10")
    static Object doCached(
        BoxingAtom atom,
        int index,
        @Cached("atom.getConstructor().getBoxedLayout()") Layout cachedLayout,
        @Cached("index") int cachedIndex,
        @Cached(value = "cachedLayout.buildGetter(cachedIndex)") FieldGetterNode getter) {
      return getter.execute(atom);
    }

    @Specialization(replaces = "doCached")
    static Object doUncached(BoxingAtom atom, int index) {
      return atom.getConstructor().getBoxedLayout().getUncachedFieldGetter(index).execute(atom);
    }
  }

  @ExportMessage
  void setField(int index, Object value) {
    fields[index] = value;
  }

  private static class InstantiatorNode extends UnboxingAtom.InstantiatorNode {
    @Override
    public Atom execute(AtomConstructor constructor, Layout layout, Object[] args) {
      return new BoxingAtom(constructor, args);
    }
  }

  private static final class GetterNode extends FieldGetterNode {
    private final int index;

    GetterNode(int index) {
      this.index = index;
    }

    @Override
    public Object execute(Atom atom) {
      return ((BoxingAtom) atom).fields[index];
    }
  }

  private static final class SetterNode extends FieldSetterNode {
    private final int index;

    SetterNode(int index) {
      this.index = index;
    }

    @Override
    public void execute(Atom atom, Object value) {
      ((BoxingAtom) atom).setField(index, value);
    }
  }

  private static final class InstantiatorFactory
      implements NodeFactory<UnboxingAtom.InstantiatorNode> {

    @Override
    public UnboxingAtom.InstantiatorNode createNode(Object... arguments) {
      assert arguments.length == 0;
      return new InstantiatorNode();
    }

    @Override
    public Class<UnboxingAtom.InstantiatorNode> getNodeClass() {
      throw new UnsupportedOperationException("Not supported yet."); // Generated from
      // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public List<List<Class<?>>> getNodeSignatures() {
      throw new UnsupportedOperationException("Not supported yet."); // Generated from
      // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public List<Class<? extends Node>> getExecutionSignature() {
      throw new UnsupportedOperationException("Not supported yet."); // Generated from
      // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public UnboxingAtom.InstantiatorNode getUncachedInstance() {
      return new InstantiatorNode();
    }
  }

  private static final class GetterFactory implements NodeFactory<FieldGetterNode> {
    private final int index;

    private GetterFactory(int index) {
      this.index = index;
    }

    @Override
    public FieldGetterNode createNode(Object... arguments) {
      return new GetterNode(index);
    }

    @Override
    public Class<FieldGetterNode> getNodeClass() {
      throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<List<Class<?>>> getNodeSignatures() {
      throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<Class<? extends Node>> getExecutionSignature() {
      throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public FieldGetterNode getUncachedInstance() {
      return new GetterNode(index);
    }
  }

  private static final class SetterFactory implements NodeFactory<FieldSetterNode> {
    private final int index;

    private SetterFactory(int index) {
      this.index = index;
    }

    @Override
    public FieldSetterNode createNode(Object... arguments) {
      return new SetterNode(index);
    }

    @Override
    public Class<FieldSetterNode> getNodeClass() {
      throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<List<Class<?>>> getNodeSignatures() {
      throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<Class<? extends Node>> getExecutionSignature() {
      throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public FieldSetterNode getUncachedInstance() {
      return new SetterNode(index);
    }
  }
}
