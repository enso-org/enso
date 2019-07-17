package org.enso.interpreter.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.runtime.errors.ArityException;

import java.util.List;

public class AtomConstructor implements TruffleObject {
  public static final AtomConstructor CONS = new AtomConstructor("Cons", 2);
  public static final AtomConstructor NIL = new AtomConstructor("Nil", 0);
  public static final AtomConstructor UNIT = new AtomConstructor("Unit", 0);

  private final String name;
  private final int arity;
  private final Atom cachedInstance;

  public AtomConstructor(String name, List<String> argNames) {
    this(name, argNames.size());
  }

  public AtomConstructor(String name, int arity) {
    this.name = name;
    this.arity = arity;
    if (arity == 0) {
      cachedInstance = new Atom(this);
    } else {
      cachedInstance = null;
    }
  }

  public String getName() {
    return name;
  }

  public int getArity() {
    return arity;
  }

  public Atom newInstance(Object... arguments) {
    if (arguments.length != arity)
      throw new ArityException(arity, arguments.length);
    if (cachedInstance != null) return cachedInstance;
    return new Atom(this, arguments);
  }

  @Override
  public String toString() {
    return super.toString() + "<" + name + "/" + arity + ">";
  }
}
