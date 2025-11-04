package org.enso.interpreter.runtime.builtin;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.enso.compiler.core.CompilerError;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.ModuleScopeBuilder;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.util.CachingSupplier;

/**
 * Holds information about loaded in builtins. This class holds two kinds of information:
 *
 * <ul>
 *   <li>Static fields contain general information about builtins. Their names, their implementation
 *       classes, etc.
 *   <li>Instance of {@link BuiltinsRegistry} holds information associated with a particular {@link
 *       EnsoContext}
 * </ul>
 */
final class BuiltinsRegistry {

  private static final List<Constructor<? extends Builtin>> loadedBuiltinConstructors;
  private static final Map<String, LoadedBuiltinMetaMethod> loadedBuiltinMethodsMeta;
  private static final Map<String, LoadedBuiltinMethod> loadedBuiltinMethods;

  static {
    loadedBuiltinConstructors = readBuiltinTypes();
    loadedBuiltinMethodsMeta = readBuiltinMethodsMeta();
    loadedBuiltinMethods = loadBuiltinMethodClassesEarly(loadedBuiltinMethodsMeta);
  }

  private final Map<String, Map<String, Supplier<LoadedBuiltinMethod>>> builtinMethodNodes;
  private final Map<Class<? extends Builtin>, Builtin> builtins;
  private final Map<String, Builtin> builtinsByName;

  BuiltinsRegistry(EnsoLanguage language, ModuleScopeBuilder scopeBuilder) {
    builtins = initializeBuiltinTypes(loadedBuiltinConstructors, language, scopeBuilder);
    builtinMethodNodes = registerBuiltinMethodsLazily(scopeBuilder, language);
    builtinsByName =
        builtins.values().stream()
            .collect(
                Collectors.toMap(
                    v -> v.getType().getName(), java.util.function.Function.identity()));
  }

  final <T extends Builtin> T getBuiltinType(Class<T> clazz) {
    var t = builtins.get(clazz);
    return clazz.cast(t);
  }

  final Builtin getByRepresentationType(Class<?> clazz) {
    for (var b : builtins.values()) {
      if (b.isRepresentedBy(clazz)) {
        return b;
      }
    }
    return null;
  }

  final Builtin getBuiltinType(String name) {
    return builtinsByName.get(name);
  }

  /**
   * Returns a builtin method for the provided Atom Constructor and the name, if it exists.
   *
   * @param type Atom Constructor owner of the function
   * @param methodName Name of the method
   * @param language The language the resulting function nodes should be associated with
   * @return A non-empty function under the given name, if it exists. An empty value if no such
   *     builtin method was ever registerd
   */
  final Optional<BuiltinFunction> getBuiltinFunction(
      String type, String methodName, EnsoLanguage language) {
    // TODO: move away from String mapping once Builtins is gone
    Map<String, Supplier<LoadedBuiltinMethod>> atomNodes = builtinMethodNodes.get(type);
    if (atomNodes == null) {
      return Optional.empty();
    }
    var supply = atomNodes.get(methodName);
    if (supply == null) {
      return Optional.empty();
    }
    LoadedBuiltinMethod builtin = supply.get();
    if (builtin == null) {
      return Optional.empty();
    }
    return builtin.toFunction(language);
  }

  private static Map<String, LoadedBuiltinMethod> loadBuiltinMethodClassesEarly(
      Map<String, LoadedBuiltinMetaMethod> map) {
    Map<String, LoadedBuiltinMethod> methods = new HashMap<>();
    map.forEach(
        (key, value) -> {
          methods.put(key, value.toMethod());
        });
    return methods;
  }

  /**
   * Returns a list of supported builtins.
   *
   * <p>Builtin types are marked via @BuiltinType annotation. The metadata file represents a single
   * builtin type per row. The format of the row is as follows: <Enso name of the builtin
   * type>:<Name of the class representing it>:[<field1>,<field2>,...] where the last column gives a
   * list of optional type's fields.
   */
  private static List<Constructor<? extends Builtin>> readBuiltinTypes() {
    ClassLoader classLoader = Builtins.class.getClassLoader();
    List<String> lines;
    final String NODE_PKG = "org.enso.interpreter.node.expression.builtin";
    final String META_PATH =
        "META-INF" + "/" + NODE_PKG.replace('.', '/') + "/BuiltinTypes.metadata";
    try (InputStream resource = classLoader.getResourceAsStream(META_PATH)) {
      lines =
          new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8))
              .lines()
              .collect(Collectors.toList());
    } catch (Exception ioe) {
      lines = new ArrayList<>();
      ioe.printStackTrace();
    }

    return lines.stream()
        .map(
            line -> {
              String[] builtinMeta = line.split(":");
              if (builtinMeta.length < 2 || builtinMeta.length > 4) {
                java.lang.System.out.println(Arrays.toString(builtinMeta));
                throw new CompilerError("Invalid builtin metadata in: " + line);
              }
              try {
                @SuppressWarnings("unchecked")
                Class<? extends Builtin> clazz =
                    (Class<? extends Builtin>) Class.forName(builtinMeta[1]);

                // Note: Don't create a new instance of the builtin at this point
                // because that will be too much for the inliner and won't get
                // constant folded.
                return clazz.getConstructor();
              } catch (ClassNotFoundException | NoSuchMethodException e) {
                e.printStackTrace();
                throw new CompilerError("Invalid builtin type entry: " + builtinMeta[1]);
              }
            })
        .collect(Collectors.toList());
  }

  /** Initialize builting types in the context of the given language and module scope */
  private Map<Class<? extends Builtin>, Builtin> initializeBuiltinTypes(
      List<Constructor<? extends Builtin>> constrs,
      EnsoLanguage language,
      ModuleScopeBuilder scope) {
    Map<Class<? extends Builtin>, Builtin> builtins = new HashMap<>();

    for (var constr : constrs) {
      try {
        Builtin builtin = constr.newInstance();
        builtins.put(builtin.getClass(), builtin);
      } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
        throw new CompilerError("Invalid builtin type entry: " + constr, e);
      }
    }
    for (var b : builtins.values()) {
      b.initialize(language, scope, builtins);
    }
    return builtins;
  }

  /**
   * Loads a Map of builtin methods.
   *
   * <p>Builtin methods are marked via @BuiltinMethod annotation. THe metadata file represents a
   * single builtin method per row. The format of the row is as follows: <Fully qualified name of
   * the builtin method>:<Class name of the builtin method representing it>
   *
   * @return A map of builtin method nodes per builtin type name
   */
  private static Map<String, LoadedBuiltinMetaMethod> readBuiltinMethodsMeta() {
    ClassLoader classLoader = Builtins.class.getClassLoader();
    List<String> lines;

    final String NODE_PKG = "org.enso.interpreter.node.expression.builtin";
    final String META_PATH =
        "META-INF" + "/" + NODE_PKG.replace('.', '/') + "/BuiltinMethods.metadata";
    try (InputStream resource = classLoader.getResourceAsStream(META_PATH)) {
      lines =
          new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8))
              .lines()
              .collect(Collectors.toList());
    } catch (Exception ioe) {
      lines = new ArrayList<>();
      ioe.printStackTrace();
    }

    return lines.stream()
        .map(
            line -> {
              String[] builtinMeta = line.split(":");
              if (builtinMeta.length != 4) {
                throw new CompilerError("Invalid builtin metadata in: " + line);
              }
              String[] builtinName = builtinMeta[0].split("\\.");
              if (builtinName.length != 2) {
                throw new CompilerError("Invalid builtin metadata in : " + line);
              }
              boolean isStatic = java.lang.Boolean.valueOf(builtinMeta[2]);
              boolean isAutoRegister = java.lang.Boolean.valueOf(builtinMeta[3]);

              return new AbstractMap.SimpleEntry<>(
                  builtinMeta[0],
                  new LoadedBuiltinMetaMethod(builtinMeta[1], isStatic, isAutoRegister));
            })
        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  /**
   * Register builtin methods and initialize them lazily in the provided scope.
   *
   * @param scope Builtins scope
   * @param language The language the resulting function nodes should be associated with
   * @return map from types to builtin methods
   */
  private Map<String, Map<String, Supplier<LoadedBuiltinMethod>>> registerBuiltinMethodsLazily(
      ModuleScopeBuilder scope, EnsoLanguage language) {
    Map<String, Map<String, Supplier<LoadedBuiltinMethod>>> builtinMethodNodes = new HashMap<>();
    Map<String, Map<String, LoadedBuiltinMetaMethod>> builtinMetaMethods = new HashMap<>();
    loadedBuiltinMethodsMeta.forEach(
        (fullName, meta) -> {
          String[] builtinName = fullName.split("\\.");
          if (builtinName.length != 2) {
            throw new CompilerError("Invalid builtin metadata for " + fullName);
          }
          String builtinMethodOwner = builtinName[0];
          String builtinMethodName = builtinName[1];
          var constr = scope.getType(builtinMethodOwner, true);
          if (constr != null) {
            Map<String, Supplier<LoadedBuiltinMethod>> atomNodes =
                getOrUpdate(builtinMethodNodes, constr.getName());
            atomNodes.put(builtinMethodName, CachingSupplier.wrap(meta::toMethod));

            Map<String, LoadedBuiltinMetaMethod> atomNodesMeta =
                getOrUpdate(builtinMetaMethods, constr.getName());
            atomNodesMeta.put(builtinMethodName, meta);
          } else {
            Map<String, Supplier<LoadedBuiltinMethod>> atomNodes =
                getOrUpdate(builtinMethodNodes, builtinMethodOwner);
            atomNodes.put(builtinMethodName, CachingSupplier.wrap(meta::toMethod));

            Map<String, LoadedBuiltinMetaMethod> atomNodesMeta =
                getOrUpdate(builtinMetaMethods, builtinMethodOwner);
            atomNodesMeta.put(builtinMethodName, meta);
          }
        });

    for (Builtin builtin : builtins.values()) {
      var type = builtin.getType();
      Map<String, LoadedBuiltinMetaMethod> methods = builtinMetaMethods.get(type.getName());
      if (methods != null) {
        // Register a builtin method iff it is marked as auto-register.
        // Methods can only register under a type or, if we deal with a static method, it's
        // eigen-type.
        // Such builtins are available on certain types without importing the whole stdlib, e.g. Any
        // or Number.
        methods.forEach(
            (key, value) -> {
              Type tpe =
                  value.isAutoRegister() ? (!value.isStatic() ? type : type.getEigentype()) : null;
              if (tpe != null) {
                Supplier<Function> supplier =
                    () -> value.toMethod().toFunction(language).get().getFunction();
                scope.registerMethod(tpe, key, supplier);
              }
            });
      }
    }
    return builtinMethodNodes;
  }

  private <T> Map<String, T> getOrUpdate(Map<String, Map<String, T>> map, String key) {
    Map<String, T> entry = map.get(key);
    if (entry == null) {
      entry = new HashMap<>();
      map.put(key, entry);
    }
    return entry;
  }

  private static class LoadedBuiltinMetaMethod {

    private LoadedBuiltinMethod method;
    private final String className;
    private final boolean staticMethod;
    private final boolean autoRegister;

    private LoadedBuiltinMetaMethod(String className, boolean staticMethod, boolean autoRegister) {
      this.className = className;
      this.staticMethod = staticMethod;
      this.autoRegister = autoRegister;
      this.method = null;
    }

    boolean isStatic() {
      return staticMethod;
    }

    boolean isAutoRegister() {
      return autoRegister;
    }

    LoadedBuiltinMethod toMethod() {
      if (method == null) {
        try {
          @SuppressWarnings("unchecked")
          Class<BuiltinRootNode> clazz = (Class<BuiltinRootNode>) Class.forName(className);
          Method meth = clazz.getMethod("makeFunction", EnsoLanguage.class);
          method = new LoadedBuiltinMethod(meth, staticMethod, autoRegister);
        } catch (ClassNotFoundException | NoSuchMethodException e) {
          throw new CompilerError("Invalid builtin method " + className, e);
        }
      }
      return method;
    }
  }

  private record LoadedBuiltinMethod(Method meth, boolean isStatic, boolean isAutoRegister) {
    Optional<BuiltinFunction> toFunction(EnsoLanguage language) {
      try {
        var f = (Function) meth.invoke(null, language);
        if (f != null) {
          var bf = new BuiltinFunction(f, isAutoRegister);
          return Optional.of(bf);
        }
      } catch (Exception e) {
        e.printStackTrace();
      }
      return Optional.empty();
    }
  }
}
