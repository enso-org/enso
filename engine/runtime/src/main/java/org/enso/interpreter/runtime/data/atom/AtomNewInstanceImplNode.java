package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.ExplodeLoop;

/**
 * This acts as the main method of instantiating unboxing atoms throughout the system. It will try
 * to reuse a known layout if it fits the data, or allocate a new one. The list of allocated layouts
 * is stored in the constructor. This way we can better reuse them, allowing for less splitting
 * throughout the system. This node will allocate these layouts, until a limit is reached, at which
 * point it will fall back onto {@link AtomConstructor#getBoxedLayout()} for data that does not fit
 * the known layouts.
 */
final class AtomNewInstanceImplNode extends AtomNewInstanceNode {

  private static final int MAX_UNBOXING_LAYOUTS = 10;
  private static final UnboxingAtom.DirectCreateLayoutInstanceNode[] EMPTY =
      new UnboxingAtom.DirectCreateLayoutInstanceNode[0];
  @Child UnboxingAtom.DirectCreateLayoutInstanceNode boxedLayout;
  @Children UnboxingAtom.DirectCreateLayoutInstanceNode[] unboxedLayouts;
  final int arity;
  @CompilerDirectives.CompilationFinal private boolean constructorAtCapacity;
  final AtomConstructor constructor;

  AtomNewInstanceImplNode(AtomConstructor constructor) {
    this.constructor = constructor;
    this.arity = constructor.getArity();
    this.boxedLayout =
        new UnboxingAtom.DirectCreateLayoutInstanceNode(constructor, constructor.getBoxedLayout());
    unboxedLayouts = EMPTY;
    updateFromConstructor();
  }

  public static AtomNewInstanceImplNode create(AtomConstructor constructor) {
    return new AtomNewInstanceImplNode(constructor);
  }

  @Override
  @ExplodeLoop
  public Atom execute(Object[] arguments) {
    long flags = computeFlags(arguments);
    if (flags == 0) {
      return boxedLayout.execute(arguments);
    }
    for (int i = 0; i < unboxedLayouts.length; i++) {
      if (unboxedLayouts[i].layout.inputFlags == flags) {
        return unboxedLayouts[i].execute(arguments);
      }
    }
    if (!constructorAtCapacity) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      org.enso.interpreter.runtime.data.atom.Layout[] layouts = constructor.getUnboxingLayouts();
      if (layouts.length == this.unboxedLayouts.length) {
        // Layouts stored in this node are probably up-to-date; create a new one and try to
        // register it.
        // var newLayout = Layout.create(arity, flags, boxedLayout.layout.args);
        var newLayout = boxedLayout.layout.copy(flags);
        constructor.atomicallyAddLayout(newLayout, this.unboxedLayouts.length);
      }
      updateFromConstructor();
      return execute(arguments);
    }
    return boxedLayout.execute(arguments);
  }

  void updateFromConstructor() {
    if (Layout.isAritySupported(arity)) {
      org.enso.interpreter.runtime.data.atom.Layout[] layouts = constructor.getUnboxingLayouts();
      org.enso.interpreter.runtime.data.atom.UnboxingAtom.DirectCreateLayoutInstanceNode[]
          newLayouts = new UnboxingAtom.DirectCreateLayoutInstanceNode[layouts.length];
      System.arraycopy(unboxedLayouts, 0, newLayouts, 0, unboxedLayouts.length);
      for (int i = unboxedLayouts.length; i < newLayouts.length; i++) {
        newLayouts[i] = new UnboxingAtom.DirectCreateLayoutInstanceNode(constructor, layouts[i]);
      }
      if (layouts.length >= MAX_UNBOXING_LAYOUTS) {
        constructorAtCapacity = true;
      }
      unboxedLayouts = newLayouts;
    } else {
      constructorAtCapacity = true;
    }
  }

  @ExplodeLoop
  long computeFlags(Object[] arguments) {
    long flags = 0;
    if (Layout.isAritySupported(arity)) {
      for (int i = 0; i < arity; i++) {
        if (arguments[i] instanceof Double) {
          flags |= Layout.Flags.DOUBLE_MASK << (i * 2);
        } else if (arguments[i] instanceof Long) {
          flags |= Layout.Flags.LONG_MASK << (i * 2);
        }
      }
    }
    return flags;
  }
}
