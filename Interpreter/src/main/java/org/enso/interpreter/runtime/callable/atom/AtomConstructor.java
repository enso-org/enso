package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.runtime.callable.Callable;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.error.ArityException;

/** A representation of an Atom constructor. */
public class AtomConstructor extends Callable implements TruffleObject {
  public static final AtomConstructor CONS =
      new AtomConstructor(
          "Cons",
          new ArgumentDefinition[] {
            new ArgumentDefinition(0, "head"), new ArgumentDefinition(1, "rest")
          });
  public static final AtomConstructor NIL = new AtomConstructor("Nil", new ArgumentDefinition[0]);
  public static final AtomConstructor UNIT = new AtomConstructor("Unit", new ArgumentDefinition[0]);

  private final String name;
  private final Atom cachedInstance;

  /**
   * Creates a new Atom constructor for a given name and with the provided arguments.
   *
   * @param name the name of the Atom constructor
   * @param args the fields associated with the constructor
   */
  public AtomConstructor(String name, ArgumentDefinition[] args) {
    super(args);
    this.name = name;
    if (args.length == 0) {
      cachedInstance = new Atom(this);
    } else {
      cachedInstance = null;
    }
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
    return getArgs().length;
  }

  /**
   * Creates a new runtime instance of the Atom represented by this constructor.
   *
   * @param arguments the runtime arguments to the constructor
   * @return a new instance of the atom represented by this constructor
   */
  public Atom newInstance(Object... arguments) {
    if (arguments.length > getArity()) {
      throw new ArityException(getArity(), arguments.length);
    }
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
}
