package org.enso.compiler.dump.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;
import java.util.List;
import org.enso.compiler.core.ir.Empty;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Name.MethodReference;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.docs.BindingSorter;
import org.enso.persist.Persistance.Reference;
import org.junit.Test;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters;

public final class BindingSorterTest {
  @Test
  public void compareTwoModuleMethods() {
    var method1 = method(null, "method_a");
    var method2 = method(null, "method_b");
    var sorted = sortBindings(method2, method1);
    assertThat("Nothing is dropped", sorted.size(), is(2));
    assertThat("method1 is first", sorted.get(0), is(method1));
    assertThat("method2 is second", sorted.get(1), is(method2));
  }

  @Test
  public void comparePrivateAndPublicMethod() {
    var publicMethod = method(null, "method_XXX", false, false);
    var privMethod = method(null, "AAAA", false, true);
    var sorted = sortBindings(privMethod, publicMethod);
    assertThat(sorted.get(0), is(publicMethod));
    assertThat(sorted.get(1), is(privMethod));
  }

  @Test
  public void compareTypeAndModuleMethod() {
    var type = type("My_Type");
    var method = method(null, "AAA");
    var sorted = sortBindings(method, type);
    assertThat(sorted.get(0), is(type));
    assertThat(sorted.get(1), is(method));
  }

  @Test
  public void compareInstanceMethodAndType() {
    var aType = type("A_Type");
    var aTypeMethod = method("A_Type", "method");
    var zType = type("Z_Type");
    var sorted = sortBindings(zType, aTypeMethod, aType);
    var expected = List.of(aType, aTypeMethod, zType);
    assertSameItems(expected, sorted);
  }

  @Test
  public void compareInstanceMethods_OnDifferentTypes() {
    var method1 = method("A_Type", "method");
    var method2 = method("Z_Type", "method");
    var sorted = sortBindings(method2, method1);
    assertThat(sorted.get(0), is(method1));
    assertThat(sorted.get(1), is(method2));
  }

  @Test
  public void compareInstanceMethods_InSameType() {
    var method1 = method("Type", "method_A");
    var method2 = method("Type", "method_B");
    var sorted = sortBindings(method2, method1);
    assertThat(sorted.get(0), is(method1));
    assertThat(sorted.get(1), is(method2));
  }

  @Test
  public void compareTypes() {
    var type1 = type("AA_Type");
    var type2 = type("XX_Type");
    var sorted = sortBindings(type2, type1);
    assertThat(sorted.get(0), is(type1));
    assertThat(sorted.get(1), is(type2));
  }

  @Test
  public void compareConstructors_InSameType() {
    var constructor1 = constructor("A_Cons");
    var constructor2 = constructor("B_Cons");
    var sorted = sortConstructors(constructor2, constructor1);
    assertThat(sorted.get(0), is(constructor1));
    assertThat(sorted.get(1), is(constructor2));
  }

  @Test
  public void compareInstanceMethodAndModuleMethod() {
    var moduleMethod = method(null, "AA", false, false);
    var type = type("My_Type");
    var instanceMethod = method("My_Type", "XX", false, false);
    var sorted = sortBindings(instanceMethod, moduleMethod, type);
    assertThat(sorted.get(0), is(type));
    assertThat(sorted.get(1), is(instanceMethod));
    assertThat(sorted.get(2), is(moduleMethod));
  }

  @Test
  public void compareInstanceMethodAndExtensionMethod() {
    var type = type("My_Type");
    var instanceMethod = method("My_Type", "XX");
    var extensionMethod = method("Any", "AA");
    var sorted = sortBindings(extensionMethod, instanceMethod, type);
    assertThat(sorted.get(0), is(type));
    assertThat(sorted.get(1), is(instanceMethod));
    assertThat(sorted.get(2), is(extensionMethod));
  }

  @Test
  public void compareInstanceMethodAndConversionMethod() {
    var type = type("My_Type");
    var instanceMethod = method("My_Type", "AA");
    var conversionMethod = conversionMethod("My_Type", "Any");
    var sorted = sortBindings(conversionMethod, instanceMethod, type);
    assertThat(sorted.get(0), is(type));
    assertThat(sorted.get(1), is(instanceMethod));
    assertThat(sorted.get(2), is(conversionMethod));
  }

  @Test
  public void compareModuleMethodAndConversionMethod() {
    var moduleMethod = method(null, "AA");
    var conversionMethod = conversionMethod("My_Type", "Any");
    var sorted = sortBindings(conversionMethod, moduleMethod);
    assertThat(sorted.get(0), is(moduleMethod));
    assertThat(sorted.get(1), is(conversionMethod));
  }

  private static <T, U> void assertSameItems(List<T> expected, List<U> actual) {
    var expectedArr = expected.toArray();
    assertThat(actual, contains(expectedArr));
  }

  private static List<Definition> sortBindings(Definition... items) {
    var modIr = module(items);
    return BindingSorter.sortedBindings(modIr);
  }

  private static List<Definition.Data> sortConstructors(Definition.Data... items) {
    return BindingSorter.sortConstructors(Arrays.stream(items).toList());
  }

  private static Module module(Definition... bindings) {
    var bindingsList = Arrays.asList(bindings);
    return new Module(
        emptyScalaList(),
        emptyScalaList(),
        CollectionConverters.asScala(bindingsList).toList(),
        false,
        null,
        new MetadataStorage());
  }

  private static Method.Explicit method(String typeName, String methodName) {
    return method(typeName, methodName, false, false);
  }

  private static Method.Explicit method(
      String typeName, String methodName, boolean isStatic, boolean isPrivate) {
    MethodReference methodRef;
    if (typeName != null) {
      methodRef =
          new Name.MethodReference(
              Option.apply(name(typeName, false)),
              name(methodName, true),
              null,
              new MetadataStorage());
    } else {
      methodRef =
          new Name.MethodReference(
              Option.empty(), name(methodName, true), null, new MetadataStorage());
    }
    Reference<Expression> bodyRef = Reference.of(empty());
    return new Method.Explicit(
        methodRef, bodyRef, isStatic, isPrivate, false, null, new MetadataStorage());
  }

  private static Method.Conversion conversionMethod(String targetTypeName, String sourceTypeName) {
    var methodRef =
        new Name.MethodReference(
            Option.apply(name(targetTypeName, false)),
            name("from", true),
            null,
            new MetadataStorage());
    return new Method.Conversion(
        methodRef, name(sourceTypeName, false), empty(), null, new MetadataStorage());
  }

  private static Name name(String nm, boolean isMethod) {
    return new Name.Literal(nm, isMethod, null, Option.empty(), new MetadataStorage());
  }

  private static Definition.Data constructor(String name) {
    return new Definition.Data(
        name(name, false), emptyScalaList(), emptyScalaList(), false, null, new MetadataStorage());
  }

  private static Definition.Type type(String name, List<Definition.Data> constructors) {
    return new Definition.Type(
        name(name, false),
        emptyScalaList(),
        CollectionConverters.asScala(constructors).toList(),
        null,
        new MetadataStorage());
  }

  private static Definition.Type type(String name) {
    return type(name, List.of());
  }

  private static Empty empty() {
    return new Empty(null);
  }

  private static <T> scala.collection.immutable.List<T> emptyScalaList() {
    return scala.collection.immutable.List$.MODULE$.empty();
  }
}
