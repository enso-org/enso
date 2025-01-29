package org.enso.compiler.docs;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Definition.Data;
import org.enso.compiler.core.ir.module.scope.Definition.Type;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Bindings are sorted to categories. Every category is sorted alphabetically.
 * Categories are roughly:
 * <ul>
 *   <li>Types</li>
 *   <li>Instance and static methods on types</li>
 *   <li>Module methods</li>
 *   <li>Extension and conversion methods</li>
 * </ul>
 */
public final class BindingSorter {
  private BindingSorter() {}

  /**
   * Returns sorted list of bindings defined on the given {@code moduleIr}.
   */
  public static List<Definition> sortedBindings(Module moduleIr) {
    var bindings = CollectionConverters.asJava(moduleIr.bindings());
    var comparator = new BindingComparator(moduleIr);
    return bindings.stream().sorted(comparator).toList();
  }

  public static List<Definition.Data> sortConstructors(List<Definition.Data> constructors) {
    var comparator = new ConstructorComparator();
    return constructors.stream().sorted(comparator).toList();
  }


  private static int compareTypes(Type type1, Type type2) {
    return type1.name().name().compareTo(type2.name().name());
  }


  private static final class BindingComparator implements java.util.Comparator<Definition> {
    private final Module moduleIr;
    private Set<String> typeNames;

    private BindingComparator(Module moduleIr) {
      this.moduleIr = moduleIr;
    }

    @Override
    public int compare(Definition def1, Definition def2) {
      return switch (def1) {
        case Method method1 when def2 instanceof Method methods ->
          compareMethods(method1, methods);
        case Type type1 when def2 instanceof Type type2 ->
          compareTypes(type1, type2);
        case Type type1 when def2 instanceof Method method2 -> compareTypeAndMethod(type1, method2);
        case Method method1 when def2 instanceof Type type2 ->
            -compareTypeAndMethod(type2, method1);
        default -> throw new AssertionError("unexpected type " + def1.getClass());
      };
    }

    private int compareTypeAndMethod(Type type, Method method) {
      if (method.typeName().isDefined()) {
        if (isExtensionMethod(method)) {
          return -1;
        }
        var typeName = type.name().name();
        var methodTypeName = method.typeName().get().name();
        if (typeName.equals(methodTypeName)) {
          return -1;
        } else {
          return typeName.compareTo(methodTypeName);
        }
      }
      return -1;
    }


    private int compareMethods(Method method1, Method method2) {
      return switch (method1) {
        case
            Method.Explicit explicitMethod1 when method2 instanceof Method.Explicit explicitMethod2 -> {
          if (explicitMethod1.isPrivate() != explicitMethod2.isPrivate()) {
            if (explicitMethod1.isPrivate()) {
              yield 1;
            } else {
              yield -1;
            }
          }
          if (isExtensionMethod(explicitMethod1) != isExtensionMethod(explicitMethod2)) {
            if (isExtensionMethod(explicitMethod1)) {
              yield 1;
            } else {
              yield -1;
            }
          }
          var type1 = explicitMethod1.methodReference().typePointer();
          var type2 = explicitMethod2.methodReference().typePointer();
          if (type1.isDefined() && type2.isDefined()) {
            // Both methods are instance or static methods - compare by type name
            var typeName1 = type1.get().name();
            var typeName2 = type2.get().name();
            if (typeName1.equals(typeName2)) {
              // Methods are defined on the same type
              yield explicitMethod1.methodName().name()
                  .compareTo(explicitMethod2.methodName().name());
            } else {
              yield type1.get().name().compareTo(type2.get().name());
            }
          } else if (type1.isDefined() && !type2.isDefined()) {
            // Instance or static methods on types have precedence over module methods
            yield -1;
          } else if (!type1.isDefined() && type2.isDefined()) {
            yield 1;
          }
          assert !type1.isDefined() && !type2.isDefined();
          yield explicitMethod1.methodName().name()
              .compareTo(explicitMethod2.methodName().name());
        }
        // Comparison of conversion methods is not supported.
        case Method.Conversion conversion1 when method2 instanceof Method.Conversion conversion2 ->
            0;
        case Method.Explicit explicit when method2 instanceof Method.Conversion -> -1;
        case Method.Conversion conversion when method2 instanceof Method.Explicit -> 1;
        default -> throw new AssertionError(
            "Unexpected type: method1=%s, method2=%s".formatted(method1.getClass(),
                method2.getClass()));
      };
    }

    /**
     * An extension method is a method that is defined on a type that is defined outside the
     * current module.
     */
    private boolean isExtensionMethod(Method method) {
      if (method.typeName().isDefined()) {
        var typeName = method.typeName().get().name();
        return !typeNamesInModule().contains(typeName);
      }
      return false;
    }

    private Set<String> typeNamesInModule() {
      if (typeNames == null) {
        typeNames = new HashSet<>();
        moduleIr.bindings().foreach(binding -> {
          if (binding instanceof Definition.Type type) {
            typeNames.add(type.name().name());
          }
          return null;
        });
      }
      return typeNames;
    }
  }

  private static final class ConstructorComparator implements java.util.Comparator<Definition.Data> {

    @Override
    public int compare(Data cons1, Data cons2) {
      return cons1.name().name().compareTo(cons2.name().name());
    }
  }
}
