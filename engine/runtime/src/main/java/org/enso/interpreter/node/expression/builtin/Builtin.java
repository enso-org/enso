package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

/** A base class for all classes annotated with @BuiltinType */
public abstract class Builtin {
  public record Cons(String name, List<String> params) {
    public Cons(String name, String... params) {
      this(name, Arrays.asList(params));
    }

    private AtomConstructor build(ModuleScope scope) {
      var res = new AtomConstructor(name, scope, true);
      res.initializeFields(
          IntStream.range(0, params.size())
              .mapToObj(
                  i ->
                      new ArgumentDefinition(
                          i, params.get(i), ArgumentDefinition.ExecutionMode.EXECUTE))
              .toArray(ArgumentDefinition[]::new));
      return res;
    }
  }

  private String name;

  private @CompilerDirectives.CompilationFinal Type type;
  private @CompilerDirectives.CompilationFinal(dimensions = 1) AtomConstructor[] constructors;
  private @CompilerDirectives.CompilationFinal AtomConstructor uniqueConstructor;

  public final void setName(String name) {
    this.name = name;
  }

  protected Class<? extends Builtin> getSuperType() {
    return Any.class;
  }

  protected List<Cons> getDeclaredConstructors() {
    return List.of();
  }

  public final void initialize(ModuleScope scope, Map<Class<? extends Builtin>, Builtin> builtins) {
    if (type == null) {
      Type supertype = null;
      if (getSuperType() != null) {
        var s = builtins.get(getSuperType());
        s.initialize(scope, builtins);
        supertype = s.getType();
      }
      type = new Type(name, scope, supertype, true);
    }
    if (constructors == null) {
      var conses = getDeclaredConstructors();
      constructors = new AtomConstructor[conses.size()];
      for (int i = 0; i < constructors.length; i++) {
        constructors[i] = conses.get(i).build(scope);
      }
      if (constructors.length == 1) {
        uniqueConstructor = constructors[0];
      }
    }
  }

  public Type getType() {
    return type;
  }

  public AtomConstructor[] getConstructors() {
    return constructors;
  }

  public AtomConstructor getUniqueConstructor() {
    return uniqueConstructor;
  }
}
