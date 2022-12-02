package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
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

    private AtomConstructor build(ModuleScope scope, Type type) {
      var res = new AtomConstructor(name, scope, type,true);
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

  private final String name;

  public Builtin() {
    name = this.getClass().getSimpleName().replaceAll("([^_A-Z])([A-Z])", "$1_$2");

  }

  private @CompilerDirectives.CompilationFinal Type type;
  private @CompilerDirectives.CompilationFinal(dimensions = 1) AtomConstructor[] constructors;

  protected Class<? extends Builtin> getSuperType() {
    return Any.class;
  }

  protected List<Cons> getDeclaredConstructors() {
    return List.of();
  }

  public final void initialize(EnsoLanguage language, ModuleScope scope, Map<Class<? extends Builtin>, Builtin> builtins) {
    if (type == null) {
      Type supertype = null;
      if (getSuperType() != null) {
        var s = builtins.get(getSuperType());
        s.initialize(language, scope, builtins);
        supertype = s.getType();
      }
      type = getDeclaredConstructors().size() == 0 ?
          Type.createSingleton(name, scope, supertype, true) :
          Type.create(name, scope, supertype, builtins.get(Any.class).getType(), true);
    }
    if (constructors == null) {
      var conses = getDeclaredConstructors();
      constructors = new AtomConstructor[conses.size()];
      for (int i = 0; i < constructors.length; i++) {
        var cons = conses.get(i).build(scope, type);
        constructors[i] = cons;
        type.registerConstructor(cons);
      }
    }
    type.generateGetters(language);
    postInitialize();
  }

  protected void postInitialize() {}

  protected String getName() {
    return name;
  }

  public final Type getType() {
    return type;
  }

  public final AtomConstructor[] getConstructors() {
    return constructors;
  }

}
