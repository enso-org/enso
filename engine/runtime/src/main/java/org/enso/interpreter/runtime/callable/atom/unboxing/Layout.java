package org.enso.interpreter.runtime.callable.atom.unboxing;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.dsl.atom.LayoutSpec;
import org.enso.interpreter.node.expression.atom.InstantiateNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/**
 * This class mediates the use of {@link UnboxingAtom} instances. It is responsible for describing
 * the mapping between logical and actual storage of fields. The {@link LayoutSpec} annotation will
 * also generate the necessary atom subclasses and a {@link LayoutFactory}, which exposes dynamic
 * access to the generated classes for use in the {@link #create(int, long)} method. This is quite
 * useful, as with the current parameters, we generate 180 different getter nodes and similar
 * numbers of other nodes participating in this system.
 */
@LayoutSpec(minFields = Layout.MIN_FIELDS, maxFields = Layout.MAX_FIELDS)
public class Layout {
  static final int MAX_FIELDS = 4;
  static final int MIN_FIELDS = 1;

  /** Helpers for reading compressed field data this layout holds. */
  public static class Flags {
    public static final long DOUBLE_MASK = 0b10;
    public static final long ALL_DOUBLES_MASK = 0xAAAAAAAAAAAAAAAAL;
    public static final long LONG_MASK = 0b01;
    public static final long ALL_LONGS_MASK = 0x5555555555555555L;

    public static boolean isDoubleAt(long flags, int index) {
      return (flags & (DOUBLE_MASK << (index * 2))) != 0;
    }

    public static boolean isLongAt(long flags, int index) {
      return (flags & (LONG_MASK << (index * 2))) != 0;
    }

    public static int countDoubles(long flags) {
      return Long.bitCount(flags & ALL_DOUBLES_MASK);
    }

    public static int countLongs(long flags) {
      return Long.bitCount(flags & ALL_LONGS_MASK);
    }
  }

  // this will work until 32 fields, then we need to fall back to all-unboxed
  final long inputFlags;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) int[] fieldToStorage;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) NodeFactory<
          ? extends UnboxingAtom.FieldGetterNode>[]
      fieldGetterFactories;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) UnboxingAtom.FieldGetterNode[]
      uncachedFieldGetters;

  private final @CompilerDirectives.CompilationFinal(dimensions = 1) ArgumentDefinition[] args;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) NodeFactory<
          ? extends UnboxingAtom.FieldSetterNode>[]
      fieldSetterFactories;

  private final @CompilerDirectives.CompilationFinal(dimensions = 1) UnboxingAtom.FieldSetterNode[]
      uncachedFieldSetters;
  private final @CompilerDirectives.CompilationFinal NodeFactory<
          ? extends UnboxingAtom.InstantiatorNode>
      instantiatorFactory;

  public Layout(
      long inputFlags,
      int[] fieldToStorage,
      NodeFactory<? extends UnboxingAtom.FieldGetterNode>[] fieldGetterFactories,
      NodeFactory<? extends UnboxingAtom.FieldSetterNode>[] fieldSetterFactories,
      NodeFactory<? extends UnboxingAtom.InstantiatorNode> instantiatorFactory,
      ArgumentDefinition[] args) {
    this.args = args;
    this.inputFlags = inputFlags;
    this.fieldToStorage = fieldToStorage;
    this.instantiatorFactory = instantiatorFactory;
    this.fieldGetterFactories = fieldGetterFactories;
    this.uncachedFieldGetters = new UnboxingAtom.FieldGetterNode[fieldGetterFactories.length];
    this.fieldSetterFactories = fieldSetterFactories;
    this.uncachedFieldSetters = new UnboxingAtom.FieldSetterNode[fieldSetterFactories.length];
    for (int i = 0; i < fieldSetterFactories.length; i++) {
      if (fieldSetterFactories[i] != null) {
        this.uncachedFieldSetters[i] = fieldSetterFactories[i].getUncachedInstance();
      }
    }
    for (int i = 0; i < fieldGetterFactories.length; i++) {
      this.uncachedFieldGetters[i] = fieldGetterFactories[i].getUncachedInstance();
      assert this.uncachedFieldGetters[i] != null;
      if (args[i].isSuspended()) {
        this.uncachedFieldGetters[i] =
            SuspendedFieldGetterNode.build(
                this.uncachedFieldGetters[i], this.uncachedFieldSetters[i]);
      }
    }
  }

  public static boolean isAritySupported(int arity) {
    return arity >= MIN_FIELDS && arity <= MAX_FIELDS;
  }

  /**
   * Creates a new instance for the given arity and flags. This method will figure out the
   * appropriate field reorderings and castings and select the correct atom subclass, together with
   * factories for getters, setters and instantiators.
   */
  @SuppressWarnings("unchecked")
  public static Layout create(int arity, long typeFlags, ArgumentDefinition[] args) {
    if (arity > 32) {
      throw new IllegalArgumentException("Too many fields in unboxed atom");
    }
    int numDouble = Flags.countDoubles(typeFlags);
    int numLong = Flags.countLongs(typeFlags);
    int numUnboxed = numDouble + numLong;
    int numBoxed = arity - numUnboxed;
    int[] fieldToStorage = new int[arity];
    int lastDouble = 0;
    int lastLong = numDouble;
    int lastBoxed = numUnboxed;
    for (int cur = 0; cur < arity; cur++) {
      if (Flags.isDoubleAt(typeFlags, cur)) {
        fieldToStorage[cur] = lastDouble++;
      } else if (Flags.isLongAt(typeFlags, cur)) {
        fieldToStorage[cur] = lastLong++;
      } else {
        fieldToStorage[cur] = lastBoxed++;
      }
    }

    var storageGetterFactories =
        LayoutFactory.getFieldGetterNodeFactories(numDouble, numLong, numBoxed);
    var getterFactories = new NodeFactory[arity];
    for (int i = 0; i < arity; i++) {
      getterFactories[i] = storageGetterFactories[fieldToStorage[i]];
    }

    var storageSetterFactories =
        LayoutFactory.getFieldSetterNodeFactories(numDouble, numLong, numBoxed);
    var setterFactories = new NodeFactory[arity];
    for (int i = 0; i < arity; i++) {
      setterFactories[i] = storageSetterFactories[fieldToStorage[i]];
    }

    var instantiatorFactory = LayoutFactory.getInstantiatorNodeFactory(numUnboxed, numBoxed);

    return new Layout(
        typeFlags, fieldToStorage, getterFactories, setterFactories, instantiatorFactory, args);
  }

  public UnboxingAtom.FieldGetterNode[] getUncachedFieldGetters() {
    return uncachedFieldGetters;
  }

  public UnboxingAtom.FieldGetterNode[] buildGetters() {
    var getters = new UnboxingAtom.FieldGetterNode[fieldGetterFactories.length];
    for (int i = 0; i < fieldGetterFactories.length; i++) {
      getters[i] = fieldGetterFactories[i].createNode();
      if (args[i].isSuspended()) {
        var setterOrNull = buildSetter(i);
        getters[i] = SuspendedFieldGetterNode.build(getters[i], setterOrNull);
      }
    }
    return getters;
  }

  public UnboxingAtom.FieldGetterNode getUncachedFieldGetter(int index) {
    return getUncachedFieldGetters()[index];
  }

  public UnboxingAtom.FieldGetterNode buildGetter(int index) {
    var node = fieldGetterFactories[index].createNode();
    if (args[index].isSuspended()) {
      node = SuspendedFieldGetterNode.build(node, buildSetter(index));
    }
    return node;
  }

  public UnboxingAtom.FieldSetterNode getUncachedFieldSetter(int index) {
    return uncachedFieldSetters[index];
  }

  public UnboxingAtom.FieldSetterNode buildSetter(int index) {
    var fieldSetterFactory = fieldSetterFactories[index];
    return fieldSetterFactory == null ? null : fieldSetterFactory.createNode();
  }

  public boolean isDoubleAt(int fieldIndex) {
    return Flags.isDoubleAt(inputFlags, fieldIndex);
  }

  public int arity() {
    return fieldToStorage.length;
  }

  public int[] getFieldToStorage() {
    return fieldToStorage;
  }

  public NodeFactory<? extends UnboxingAtom.InstantiatorNode> getInstantiatorFactory() {
    return instantiatorFactory;
  }

  /**
   * This acts as the main method of instantiating unboxing atoms throughout the system. It will try
   * to reuse a known layout if it fits the data, or allocate a new one. The list of allocated
   * layouts is stored in the constructor. This way we can better reuse them, allowing for less
   * splitting throughout the system. This node will allocate these layouts, until a limit is
   * reached, at which point it will fall back onto {@link AtomConstructor#getBoxedLayout()} for
   * data that does not fit the known layouts.
   */
  public static class CreateUnboxedInstanceNode extends InstantiateNode.CreateInstanceNode {
    @Child UnboxingAtom.DirectCreateLayoutInstanceNode boxedLayout;
    @Children UnboxingAtom.DirectCreateLayoutInstanceNode[] unboxedLayouts;
    private final int arity;
    private @CompilerDirectives.CompilationFinal boolean constructorAtCapacity;

    private final AtomConstructor constructor;

    CreateUnboxedInstanceNode(AtomConstructor constructor) {
      this.constructor = constructor;
      this.arity = constructor.getArity();
      this.boxedLayout =
          new UnboxingAtom.DirectCreateLayoutInstanceNode(
              constructor, constructor.getBoxedLayout());
      unboxedLayouts = new UnboxingAtom.DirectCreateLayoutInstanceNode[0];
      updateFromConstructor();
    }

    public static CreateUnboxedInstanceNode create(AtomConstructor constructor) {
      return new CreateUnboxedInstanceNode(constructor);
    }

    @Override
    @ExplodeLoop
    public Object execute(Object[] arguments) {
      var flags = computeFlags(arguments);
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
        var layouts = constructor.getUnboxingLayouts();
        if (layouts.length == this.unboxedLayouts.length) {
          // Layouts stored in this node are probably up-to-date; create a new one and try to
          // register it.
          var newLayout = Layout.create(arity, flags, boxedLayout.layout.args);
          constructor.atomicallyAddLayout(newLayout, this.unboxedLayouts.length);
        }
        updateFromConstructor();
        return execute(arguments);
      }

      return boxedLayout.execute(arguments);
    }

    void updateFromConstructor() {
      var layouts = constructor.getUnboxingLayouts();
      var newLayouts = new UnboxingAtom.DirectCreateLayoutInstanceNode[layouts.length];
      System.arraycopy(unboxedLayouts, 0, newLayouts, 0, unboxedLayouts.length);
      for (int i = unboxedLayouts.length; i < newLayouts.length; i++) {
        newLayouts[i] = new UnboxingAtom.DirectCreateLayoutInstanceNode(constructor, layouts[i]);
      }
      if (layouts.length == EnsoContext.get(this).getMaxUnboxingLayouts()) {
        constructorAtCapacity = true;
      }
      unboxedLayouts = newLayouts;
    }

    @ExplodeLoop
    long computeFlags(Object[] arguments) {
      long flags = 0;
      for (int i = 0; i < arity; i++) {
        if (arguments[i] instanceof Double) {
          flags |= Flags.DOUBLE_MASK << (i * 2);
        } else if (arguments[i] instanceof Long) {
          flags |= Flags.LONG_MASK << (i * 2);
        }
      }
      return flags;
    }
  }
}
