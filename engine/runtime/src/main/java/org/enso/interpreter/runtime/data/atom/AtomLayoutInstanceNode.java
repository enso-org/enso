package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;

/**
 * Wraps an {@link InstantiatorNode} to provide an externally usable interface for creating nodes.
 * It performs field reordering and casting based on the {@link Layout} to make sure it creates an
 * instance that can be understood based on the particular layout instance.
 */
final class AtomLayoutInstanceNode extends Node {
  final Layout layout;
  final AtomConstructor constructor;
  @Children private ReadAtIndexNode[] argReaderNodes;
  @Child private UnboxingAtom.InstantiatorNode instantiator;

  AtomLayoutInstanceNode(AtomConstructor constructor, Layout layout) {
    this.constructor = constructor;
    this.layout = layout;
    this.argReaderNodes = new ReadAtIndexNode[layout.arity()];
    for (int i = 0; i < layout.arity(); i++) {
      this.argReaderNodes[layout.getFieldToStorage()[i]] =
          ReadAtIndexNode.create(i, layout.isDoubleAt(i));
    }
    this.instantiator = layout.getInstantiatorFactory().createNode();
  }

  @ExplodeLoop
  public Atom execute(Object[] args) {
    java.lang.Object[] arguments = new Object[argReaderNodes.length];
    for (int i = 0; i < argReaderNodes.length; i++) {
      arguments[i] = argReaderNodes[i].execute(args);
    }
    return instantiator.execute(constructor, layout, arguments);
  }

  static Atom uncached(AtomConstructor cons, Layout layout, Object[] arguments) {
    var node = layout.getInstantiatorFactory().getUncachedInstance();
    if (cons.getBoxedLayout() == layout) {
      return node.execute(cons, layout, arguments);
    }
    var newArgs = new Object[arguments.length];
    var order = layout.getFieldToStorage();
    for (int i = 0; i < order.length; i++) {
      var v = arguments[i];
      newArgs[order[i]] =
          switch (v) {
            case Double d -> Double.doubleToRawLongBits(d);
            default -> v;
          };
    }
    return node.execute(cons, layout, newArgs);
  }

  abstract static class ReadAtIndexNode extends Node {

    final int index;

    public static ReadAtIndexNode create(int fieldIndex, boolean isDouble) {
      return isDouble
          ? new ReadDoubleAtIndexNode(fieldIndex)
          : new ReadObjectAtIndexNode(fieldIndex);
    }

    public ReadAtIndexNode(int index) {
      this.index = index;
    }

    public abstract Object execute(Object[] args);
  }

  static class ReadObjectAtIndexNode extends ReadAtIndexNode {

    ReadObjectAtIndexNode(int index) {
      super(index);
    }

    @Override
    public Object execute(Object[] args) {
      return args[index];
    }
  }

  static class ReadDoubleAtIndexNode extends ReadAtIndexNode {

    ReadDoubleAtIndexNode(int index) {
      super(index);
    }

    @Override
    public Object execute(Object[] args) {
      return Double.doubleToRawLongBits((double) args[index]);
    }
  }
}
