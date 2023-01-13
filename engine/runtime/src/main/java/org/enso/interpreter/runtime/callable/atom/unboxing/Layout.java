package org.enso.interpreter.runtime.callable.atom.unboxing;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.atom.LayoutSpec;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@LayoutSpec(minFields = 1, maxFields = 4)
public class Layout {

  public static class Flags {
    public static final long DOUBLE_MASK = 0b10;
    public static final long ALL_DOUBLES_MASK = 0xAAAAAAAAAAAAAAAAL;
    public static final long LONG_MASK = 0b01;
    public static final long ALL_LONGS_MASK = 0x5555555555555555L;
    public static final long UNBOXED_MASK = DOUBLE_MASK | LONG_MASK;

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
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) NodeFactory<? extends UnboxingAtom.FieldGetterNode>[] fieldGetterFactories;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) UnboxingAtom.FieldGetterNode[] uncachedFieldGetters;
  private final @CompilerDirectives.CompilationFinal NodeFactory<? extends UnboxingAtom.InstantiatorNode> instantiatorFactory;

  public Layout(long inputFlags, int[] fieldToStorage, NodeFactory<? extends UnboxingAtom.FieldGetterNode>[] fieldGetterFactories, NodeFactory<? extends UnboxingAtom.InstantiatorNode> instantiatorFactory) {
    this.inputFlags = inputFlags;
    this.fieldToStorage = fieldToStorage;
    this.fieldGetterFactories = fieldGetterFactories;
    this.uncachedFieldGetters = new UnboxingAtom.FieldGetterNode[fieldGetterFactories.length];
    this.instantiatorFactory = instantiatorFactory;
    for (int i = 0; i < fieldGetterFactories.length; i++) {
      this.uncachedFieldGetters[i] = fieldGetterFactories[i].getUncachedInstance();
      assert this.uncachedFieldGetters[i] != null;
    }
  }

  public static boolean isAritySupported(int arity) {
    return arity == 2;
  }

  public static Layout create(int arity, long typeFlags) {
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

    var storageGetterFactories = new NodeFactory[arity];

    switch (numBoxed) {
      case 0:
        switch (numUnboxed) {
          case 2:
            for (int i = 0; i < numDouble; i++) {
              storageGetterFactories[i] = Atom_0_2.getFieldGetterNodeFactory(i, true);
            }
            for (int i = numDouble; i < numUnboxed; i++) {
              storageGetterFactories[i] = Atom_0_2.getFieldGetterNodeFactory(i, false);
            }
            for (int i = numUnboxed; i < arity; i++) {
              storageGetterFactories[i] = Atom_0_2.getFieldGetterNodeFactory(i, false);
            }
            break;
          default:
            throw new IllegalArgumentException("Unsupported arity");
        }
        break;
      case 1:
        switch (numUnboxed) {
          case 1:
            for (int i = 0; i < numDouble; i++) {
              storageGetterFactories[i] = Atom_1_1.getFieldGetterNodeFactory(i, true);
            }
            for (int i = numDouble; i < numUnboxed; i++) {
              storageGetterFactories[i] = Atom_1_1.getFieldGetterNodeFactory(i, false);
            }
            for (int i = numUnboxed; i < arity; i++) {
              storageGetterFactories[i] = Atom_1_1.getFieldGetterNodeFactory(i, false);
            }
            break;
          default:
            throw new IllegalArgumentException("Unsupported arity");
        }
        break;
      case 2:
        switch (numUnboxed) {
          case 0:
            for (int i = 0; i < numDouble; i++) {
              storageGetterFactories[i] = Atom_2_0.getFieldGetterNodeFactory(i, true);
            }
            for (int i = numDouble; i < numUnboxed; i++) {
              storageGetterFactories[i] = Atom_2_0.getFieldGetterNodeFactory(i, false);
            }
            for (int i = numUnboxed; i < arity; i++) {
              storageGetterFactories[i] = Atom_2_0.getFieldGetterNodeFactory(i, false);
            }
            break;
          default:
            throw new IllegalArgumentException("Unsupported arity");
        }
        break;
      default:
        throw new IllegalArgumentException("Unsupported arity");
    }

    var getterFactories = new NodeFactory[arity];

    for (int i = 0; i < arity; i++) {
      getterFactories[i] = storageGetterFactories[fieldToStorage[i]];
    }

    var instantiatorFactory = switch (numUnboxed) {
      case 0 -> switch (numBoxed) {
        case 2 -> Atom_0_2.getInstantiatorNodeFactory();
        default -> throw new IllegalArgumentException("Unsupported arity");
      };
      case 1 -> switch (numBoxed) {
        case 1 -> Atom_1_1.getInstantiatorNodeFactory();
        default -> throw new IllegalArgumentException("Unsupported arity");
      };
      case 2 -> switch (numBoxed) {
        case 0 -> Atom_2_0.getInstantiatorNodeFactory();
        default -> throw new IllegalArgumentException("Unsupported arity");
      };
      default -> throw new IllegalArgumentException("Unsupported arity");
    };

    return new Layout(typeFlags, fieldToStorage, getterFactories, instantiatorFactory);
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

  public boolean isDoubleAt(int fieldIndex) {
    return Flags.isDoubleAt(inputFlags, fieldIndex);
  }

  public int arity() {
    return fieldToStorage.length;
  }

  static class DirectCreateLayoutInstanceNode extends Node {
    final Layout layout;
    final AtomConstructor constructor;
    private @Children ReadAtIndexNode[] argReaderNodes;
    private @Child UnboxingAtom.InstantiatorNode instantiator;

    public DirectCreateLayoutInstanceNode(AtomConstructor constructor, Layout layout) {
      this.constructor = constructor;
      this.layout = layout;
      this.argReaderNodes = new ReadAtIndexNode[layout.arity()];
      for (int i = 0; i < layout.arity(); i++) {
        this.argReaderNodes[layout.fieldToStorage[i]] = ReadAtIndexNode.create(i, layout.isDoubleAt(i));
      }
      this.instantiator = layout.instantiatorFactory.createNode();
    }

    @ExplodeLoop
    public Atom execute(Object[] args) {
      var arguments = new Object[argReaderNodes.length];
      for (int i = 0; i < argReaderNodes.length; i++) {
        arguments[i] = argReaderNodes[i].execute(args);
      }
      return instantiator.execute(constructor, layout, arguments);
    }

    static abstract class ReadAtIndexNode extends Node {
      final int index;

      public static ReadAtIndexNode create(int fieldIndex, boolean isDouble) {
        return isDouble ? new ReadDoubleAtIndexNode(fieldIndex) : new ReadObjectAtIndexNode(fieldIndex);
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
}
