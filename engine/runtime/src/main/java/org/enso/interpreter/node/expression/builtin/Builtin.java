package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A base class for all classes annotated with @BuiltinType */
public abstract class Builtin {
  public record Cons(String name, List<String> params) {
    public Cons(String name, String... params) {
      this(name, Arrays.asList(params));
    }

    private AtomConstructor build(
        EnsoLanguage language, ModuleScope.Builder builder, ModuleScope scope, Type type) {
      var res = new AtomConstructor(name, builder.getModule(), type, true);
      res.initializeBuilder(language, builder);
      res.initializeFields(
          language,
          scope,
          IntStream.range(0, params.size())
              .mapToObj(
                  i ->
                      new ArgumentDefinition(
                          i, params.get(i), null, null, ArgumentDefinition.ExecutionMode.EXECUTE))
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

  public final void initialize(
      EnsoLanguage language,
      Module module,
      ModuleScope.Builder builder,
      ModuleScope scope,
      Map<Class<? extends Builtin>, Builtin> builtins) {
    if (type == null) {
      Type supertype = null;
      if (getSuperType() != null) {
        var s = builtins.get(getSuperType());
        s.initialize(language, module, builder, scope, builtins);
        supertype = s.getType();
      }
      type =
          containsValues()
              ? Type.create(
                  name, module, builder, supertype, builtins.get(Any.class).getType(), true, false)
              : Type.createSingleton(name, module, supertype, true, false);
    }
    if (constructors == null) {
      var conses = getDeclaredConstructors();
      constructors = new AtomConstructor[conses.size()];
      for (int i = 0; i < constructors.length; i++) {
        var cons = conses.get(i).build(language, builder, scope, type);
        constructors[i] = cons;
        type.registerConstructor(cons);
      }
    }
    type.generateGetters(builder, language);
    postInitialize();
  }

  public boolean containsValues() {
    return getDeclaredConstructors().size() > 0;
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
