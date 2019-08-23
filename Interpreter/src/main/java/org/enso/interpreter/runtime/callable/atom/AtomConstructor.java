package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.argument.ReadArgumentNode;
import org.enso.interpreter.node.expression.atom.InstantiateNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.ArgumentSchema;
import org.enso.interpreter.runtime.callable.function.Function;

/** A representation of an Atom constructor. */
public class AtomConstructor implements TruffleObject {
  public static final AtomConstructor CONS =
      new AtomConstructor("Cons")
          .initializeFields(new ArgumentDefinition(0, "head"), new ArgumentDefinition(1, "rest"));
  public static final AtomConstructor NIL = new AtomConstructor("Nil").initializeFields();
  public static final AtomConstructor UNIT = new AtomConstructor("Unit").initializeFields();

  private final String name;
  private @CompilerDirectives.CompilationFinal Atom cachedInstance;
  private @CompilerDirectives.CompilationFinal Function constructorFunction;

  /**
   * Creates a new Atom constructor for a given name. The constructor is not valid until {@link
   * AtomConstructor#initializeFields(ArgumentDefinition...)} is called.
   *
   * @param name the name of the Atom constructor
   */
  public AtomConstructor(String name) {
    this.name = name;
  }

  /**
   * Sets the fields of this {@link AtomConstructor} and generates a constructor function.
   *
   * @param args the arguments this constructor will take
   * @return {@code this}, for convenience
   */
  public AtomConstructor initializeFields(ArgumentDefinition... args) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.constructorFunction = buildConstructorFunction(args);
    if (args.length == 0) {
      cachedInstance = new Atom(this);
    } else {
      cachedInstance = null;
    }
    return this;
  }

  /**
   * Generates a constructor function to be used for object instantiation from other Enso code.
   *
   * @param args the argument definitions for the constructor function to take
   * @return a {@link Function} taking the specified arguments and returning an instance for this
   *     {@link AtomConstructor}
   */
  private Function buildConstructorFunction(ArgumentDefinition[] args) {
    ExpressionNode[] argumentReaders = new ExpressionNode[args.length];
    for (int i = 0; i < args.length; i++) {
      argumentReaders[i] = new ReadArgumentNode(i, args[i].getDefaultValue().orElse(null));
    }
    ExpressionNode instantiateNode = new InstantiateNode(this, argumentReaders);
    EnsoRootNode rootNode =
        new EnsoRootNode(
            null, new FrameDescriptor(), instantiateNode, null, "<constructor>:" + name);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
    return new Function(callTarget, null, new ArgumentSchema(args));
  }

  /**
   * Gets the name of the constructor.
   *
   * @return the name of the Atom constructor
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the number of arguments expected by the constructor.
   *
   * @return the number of args expected by the constructor.
   */
  public int getArity() {
    return constructorFunction.getSchema().getArgumentsCount();
  }

  /**
   * Creates a new runtime instance of the Atom represented by this constructor.
   *
   * @param arguments the runtime arguments to the constructor
   * @return a new instance of the atom represented by this constructor
   */
  public Atom newInstance(Object... arguments) {
    if (cachedInstance != null) return cachedInstance;
    return new Atom(this, arguments);
  }

  /**
   * Creates a textual representation of this Atom constructor, useful for debugging.
   *
   * @return a textual representation of this Atom constructor
   */
  @Override
  public String toString() {
    return super.toString() + "<" + name + "/" + getArity() + ">";
  }

  public Function getConstructorFunction() {
    return constructorFunction;
  }
}
