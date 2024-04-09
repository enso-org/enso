package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.nodes.Node;
import java.util.List;
import org.enso.interpreter.dsl.atom.LayoutSpec;
import org.enso.interpreter.node.callable.argument.ReadArgumentCheckNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;

/**
 * This class mediates the use of {@link UnboxingAtom} instances. It is responsible for describing
 * the mapping between logical and actual storage of fields. The {@link LayoutSpec} annotation will
 * also generate the necessary atom subclasses and a {@link LayoutFactory}, which exposes dynamic
 * access to the generated classes for use in the {@link #create(int, long)} method. This is quite
 * useful, as with the current parameters, we generate 180 different getter nodes and similar
 * numbers of other nodes participating in this system.
 */
@LayoutSpec(minFields = Layout.MIN_FIELDS, maxFields = Layout.MAX_FIELDS)
class Layout {
  static final int MAX_FIELDS = 4;
  static final int MIN_FIELDS = 1;

  /** Helpers for reading compressed field data this layout holds. */
  static final class Flags {
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

  private Layout(
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
      if (args[i].isSuspended()) {
        this.fieldGetterFactories[i] =
            SuspendedFieldGetterNode.factory(fieldGetterFactories[i], fieldSetterFactories[i]);
      }
      this.uncachedFieldGetters[i] = fieldGetterFactories[i].getUncachedInstance();
      assert this.uncachedFieldGetters[i] != null;
    }
  }

  public static boolean isAritySupported(int arity) {
    return arity >= MIN_FIELDS && arity <= MAX_FIELDS;
  }

  final Layout copy(long typeFlags) {
    return create(args.length, typeFlags, args);
  }

  /**
   * Creates a new instance for the given arity and flags. This method will figure out the
   * appropriate field reorderings and castings and select the correct atom subclass, together with
   * factories for getters, setters and instantiators.
   */
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
    return createNew(
        isAritySupported(arity), fieldToStorage, typeFlags, numDouble, numLong, numBoxed, args);
  }

  static Layout createBoxed(ArgumentDefinition[] args) {
    var arr = new int[args.length];
    for (var i = 0; i < arr.length; i++) {
      arr[i] = i;
    }
    return createNew(false, arr, 0, 0, 0, args.length, args);
  }

  @SuppressWarnings("unchecked")
  private static Layout createNew(
      boolean useUnboxed,
      int[] fieldToStorage,
      long typeFlags,
      int numDouble,
      int numLong,
      int numBoxed,
      ArgumentDefinition[] args) {
    var numUnboxed = numDouble + numLong;
    var storageGetterFactories =
        useUnboxed
            ? LayoutFactory.getFieldGetterNodeFactories(numDouble, numLong, numBoxed)
            : BoxingAtom.getFieldGetterNodeFactories(fieldToStorage.length);
    var getterFactories = new NodeFactory[fieldToStorage.length];
    for (int i = 0; i < fieldToStorage.length; i++) {
      getterFactories[i] = storageGetterFactories[fieldToStorage[i]];
    }

    var storageSetterFactories =
        useUnboxed
            ? LayoutFactory.getFieldSetterNodeFactories(numDouble, numLong, numBoxed)
            : BoxingAtom.getFieldSetterNodeFactories(fieldToStorage.length);
    var setterFactories = new NodeFactory[fieldToStorage.length];
    for (int i = 0; i < fieldToStorage.length; i++) {
      var factory = storageSetterFactories[fieldToStorage[i]];
      var types = args[i].getCheckType();
      if (types != null && factory != null) {
        factory = new SetterTypeCheckFactory(args[i], types, factory);
      }
      setterFactories[i] = factory;
    }

    var instantiatorFactory =
        useUnboxed
            ? LayoutFactory.getInstantiatorNodeFactory(numUnboxed, numBoxed)
            : BoxingAtom.FACTORY;

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
    }
    return getters;
  }

  public UnboxingAtom.FieldGetterNode getUncachedFieldGetter(int index) {
    return getUncachedFieldGetters()[index];
  }

  public UnboxingAtom.FieldGetterNode buildGetter(int index) {
    var node = fieldGetterFactories[index].createNode();
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

  private static final class SetterTypeCheckFactory
      implements NodeFactory<UnboxingAtom.FieldSetterNode> {
    private final String argName;
    private final ReadArgumentCheckNode typeCheck;
    private final NodeFactory<UnboxingAtom.FieldSetterNode> delegate;

    private SetterTypeCheckFactory(
        ArgumentDefinition arg,
        ReadArgumentCheckNode typeCheck,
        NodeFactory<UnboxingAtom.FieldSetterNode> factory) {
      assert factory != null;
      this.argName = arg.getName();
      this.typeCheck = typeCheck;
      this.delegate = factory;
    }

    @Override
    public UnboxingAtom.FieldSetterNode createNode(Object... arguments) {
      var checkNode = (ReadArgumentCheckNode) typeCheck.copy();
      var setterNode = delegate.createNode(arguments);
      return checkNode == null ? setterNode : new CheckFieldSetterNode(setterNode, checkNode);
    }

    @Override
    public Class<UnboxingAtom.FieldSetterNode> getNodeClass() {
      return delegate.getNodeClass();
    }

    @Override
    public List<List<Class<?>>> getNodeSignatures() {
      return delegate.getNodeSignatures();
    }

    @Override
    public List<Class<? extends Node>> getExecutionSignature() {
      return delegate.getExecutionSignature();
    }

    @Override
    public UnboxingAtom.FieldSetterNode getUncachedInstance() {
      return delegate.getUncachedInstance();
    }
  }

  private static final class CheckFieldSetterNode extends UnboxingAtom.FieldSetterNode {
    @Child ReadArgumentCheckNode checkNode;
    @Child UnboxingAtom.FieldSetterNode setterNode;

    private CheckFieldSetterNode(
        UnboxingAtom.FieldSetterNode setterNode, ReadArgumentCheckNode checkNode) {
      this.setterNode = setterNode;
      this.checkNode = checkNode;
    }

    @Override
    public void execute(Atom atom, Object value) {
      var valueOrConvertedValue = checkNode.handleCheckOrConversion(null, value);
      setterNode.execute(atom, valueOrConvertedValue);
    }
  }
}
