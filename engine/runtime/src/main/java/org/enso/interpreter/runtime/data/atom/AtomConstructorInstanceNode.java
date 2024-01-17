package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;

/**
 * This acts as the main method of instantiating unboxing atoms throughout the system. It will try
 * to reuse a known layout if it fits the data, or allocate a new one. The list of allocated layouts
 * is stored in the constructor. This way we can better reuse them, allowing for less splitting
 * throughout the system. This node will allocate these layouts, until a limit is reached, at which
 * point it will fall back onto {@link AtomConstructor#getBoxedLayout()} for data that does not fit
 * the known layouts.
 */
final class AtomConstructorInstanceNode extends Node {

  private static final int MAX_UNBOXING_LAYOUTS = 10;
  private static final AtomLayoutInstanceNode[] EMPTY = new AtomLayoutInstanceNode[0];
  @Child AtomLayoutInstanceNode boxedLayout;
  @Children AtomLayoutInstanceNode[] unboxedLayouts;
  final int arity;
  @CompilerDirectives.CompilationFinal private boolean constructorAtCapacity;
  final AtomConstructor constructor;

  AtomConstructorInstanceNode(AtomConstructor constructor) {
    this.constructor = constructor;
    this.arity = constructor.getArity();
    this.boxedLayout = new AtomLayoutInstanceNode(constructor, constructor.getBoxedLayout());
    unboxedLayouts = EMPTY;
    updateFromConstructor();
  }

  @NeverDefault
  public static AtomConstructorInstanceNode create(AtomConstructor constructor) {
    return new AtomConstructorInstanceNode(constructor);
  }

  @CompilerDirectives.TruffleBoundary
  static Atom uncached(Node self, AtomConstructor cons, Object[] arguments) {
    var arity = cons.getArity();
    verifyArity(self, arity, arguments);
    var boxedLayout = cons.getBoxedLayout();
    WITH_UNBOXED:
    for (; ; ) {
      var unboxedLayouts = cons.getUnboxingLayouts();
      var flags = computeFlags(arity, arguments);
      if (flags == 0) {
        return AtomLayoutInstanceNode.uncached(cons, boxedLayout, arguments);
      }
      for (int i = 0; i < unboxedLayouts.length; i++) {
        if (unboxedLayouts[i].inputFlags == flags) {
          return AtomLayoutInstanceNode.uncached(cons, unboxedLayouts[i], arguments);
        }
      }
      if (unboxedLayouts.length < MAX_UNBOXING_LAYOUTS) {
        var newLayout = boxedLayout.copy(flags);
        cons.atomicallyAddLayout(newLayout, unboxedLayouts.length);
        continue WITH_UNBOXED;
      }
      return AtomLayoutInstanceNode.uncached(cons, boxedLayout, arguments);
    }
  }

  @ExplodeLoop
  private Atom cached(long flags, Object[] arguments) {
    for (int i = 0; i < unboxedLayouts.length; i++) {
      if (unboxedLayouts[i].layout.inputFlags == flags) {
        return unboxedLayouts[i].execute(arguments);
      }
    }
    return null;
  }

  public final Atom execute(Object[] arguments) {
    verifyArity(this, arity, arguments);
    long flags = computeFlags(arity, arguments);
    if (flags == 0) {
      return boxedLayout.execute(arguments);
    }
    WITH_UNBOXED:
    for (; ; ) {
      var atom = cached(flags, arguments);
      if (atom != null) {
        return atom;
      } else {
        if (!constructorAtCapacity) {
          CompilerDirectives.transferToInterpreterAndInvalidate();
          var layouts = constructor.getUnboxingLayouts();
          if (layouts.length == this.unboxedLayouts.length) {
            // Layouts stored in this node are probably up-to-date; create a new one and try to
            // register it.
            var newLayout = boxedLayout.layout.copy(flags);
            constructor.atomicallyAddLayout(newLayout, this.unboxedLayouts.length);
          }
          updateFromConstructor();
          continue WITH_UNBOXED;
        }
        return boxedLayout.execute(arguments);
      }
    }
  }

  void updateFromConstructor() {
    CompilerAsserts.neverPartOfCompilation();
    if (Layout.isAritySupported(arity)) {
      var layouts = constructor.getUnboxingLayouts();
      var newLayouts = new AtomLayoutInstanceNode[layouts.length];
      System.arraycopy(unboxedLayouts, 0, newLayouts, 0, unboxedLayouts.length);
      for (int i = unboxedLayouts.length; i < newLayouts.length; i++) {
        newLayouts[i] = new AtomLayoutInstanceNode(constructor, layouts[i]);
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
  private static long computeFlags(int arity, Object[] arguments) {
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

  private static void verifyArity(Node self, int arity, Object[] arguments) throws PanicException {
    if (arity != arguments.length) {
      var ctx = EnsoContext.get(self);
      var err = ctx.getBuiltins().error().makeArityError(arity, arity, arguments.length);
      throw new PanicException(err, self);
    }
  }
}
