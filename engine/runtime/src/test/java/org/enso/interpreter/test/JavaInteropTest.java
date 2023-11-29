package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class JavaInteropTest extends TestBase {

  private static Context ctx;
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @BeforeClass
  public static void prepareCtx() {
    ctx = createDefaultContext(out);
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Before
  public void resetOutput() {
    out.reset();
  }

  private String[] getStdOutLines() {
    return out
        .toString(StandardCharsets.UTF_8)
        .trim()
        .split(System.lineSeparator());
  }

  private void checkPrint(String code, List<String> expected) {
    Value result = evalModule(ctx, code);
    assertTrue("should return Nothing", result.isNull());
    assertArrayEquals(expected.toArray(), getStdOutLines());
  }

  @Test
  public void testClassImport() {
    var code = """
        polyglot java import org.enso.example.TestClass
        main = TestClass.add 1 2
        """;
    var result = evalModule(ctx, code);
    assertEquals(3, result.asInt());
  }

  @Test
  public void testClassImportAndMethodCall() {
    var code = """
        polyglot java import org.enso.example.TestClass
        main =
            instance = TestClass.new (x -> x * 2)
            instance.callFunctionAndIncrement 10
        """;
    var result = evalModule(ctx, code);
    assertEquals(21, result.asInt());
  }

  @Test
  public void testImportStaticInnerClass() {
    var code = """
        polyglot java import org.enso.example.TestClass.StaticInnerClass

        main =
            instance = StaticInnerClass.new "my_data"
            instance.add 1 2
        """;
    var result = evalModule(ctx, code);
    assertEquals(3, result.asInt());
  }

  @Test
  public void testImportInnerEnum() {
    var code = """
        from Standard.Base import IO
        polyglot java import org.enso.example.TestClass
        polyglot java import org.enso.example.TestClass.InnerEnum

        main =
            IO.println <| TestClass.enumToString InnerEnum.ENUM_VALUE_1
            IO.println <| TestClass.enumToString TestClass.InnerEnum.ENUM_VALUE_2
        """;
    checkPrint(code, List.of("ENUM_VALUE_1", "ENUM_VALUE_2"));
  }

  @Test
  public void testCaseOnEnum() {
    var code = """
        from Standard.Base import IO
        polyglot java import org.enso.example.TestClass
        polyglot java import org.enso.example.TestClass.InnerEnum

        to_string x = case x of
          InnerEnum.ENUM_VALUE_1 -> "one"
          InnerEnum.ENUM_VALUE_2 -> "two"
          _ -> "none"

        main =
            IO.println <| to_string TestClass.InnerEnum.ENUM_VALUE_1
            IO.println <| to_string TestClass.InnerEnum.ENUM_VALUE_2
        """;
    checkPrint(code, List.of("one", "two"));
  }

  @Test
  public void testCaseNonFinal() {
    var code = """
        from Standard.Base import IO
        polyglot java import org.enso.example.TestClass

        to_string x = case x of
          TestClass.FINAL_ONE -> "one"
          TestClass.nonFinalTwo -> "two"
          _ -> "none"

        main =
            IO.println <| to_string 1
            IO.println <| to_string 2
        """;
    try {
      checkPrint(code, List.of());
      fail("Expecting exception");
    } catch (PolyglotException e) {
      assertEquals("Compile error: nonFinalTwo is not a constant.", e.getMessage());
    }
  }
  @Test
  public void testShortConstant() {
    var code = """
        from Standard.Base import IO
        polyglot java import org.enso.example.TestClass

        to_string x = case x of
          TestClass.FINAL_ONE -> "int"
          TestClass.SHORT_ONE -> "short"
          TestClass.LONG_ONE -> "long"
          _ -> "none"

        main =
            IO.println <| to_string 1
            IO.println <| to_string 2
            IO.println <| to_string 3
            IO.println <| to_string 4
        """;
    checkPrint(code, List.of("int", "none", "short", "long"));
  }

  @Test
  public void testImportOuterClassAndReferenceInner() {
    var code = """
        polyglot java import org.enso.example.TestClass

        main =
            instance = TestClass.StaticInnerClass.new "my_data"
            instance.getData
        """;
    var result = evalModule(ctx, code);
    assertEquals("my_data", result.asString());
  }

  @Test
  public void testImportBothInnerAndOuterClass() {
    var code = """
        from Standard.Base import IO
        polyglot java import org.enso.example.TestClass
        polyglot java import org.enso.example.TestClass.StaticInnerClass

        main =
            inner_value = TestClass.StaticInnerClass.new "my_data"
            other_inner_value = StaticInnerClass.new "my_data"
            IO.println <| inner_value.getData
            IO.println <| other_inner_value.getData
        """;
    checkPrint(code, List.of("my_data", "my_data"));
  }

  @Test
  public void testImportNestedInnerClass() {
    var code = """
        polyglot java import org.enso.example.TestClass.StaticInnerClass.StaticInnerInnerClass

        main =
            inner_inner_value = StaticInnerInnerClass.new
            inner_inner_value.mul 3 5
        """;
    var res = evalModule(ctx, code);
    assertEquals(15, res.asInt());
  }

  @Test
  public void testImportNonExistingInnerClass() {
    var code = """
        polyglot java import org.enso.example.TestClass.StaticInnerClass.Non_Existing_Class
        """;
    try {
      evalModule(ctx, code);
      fail("Should throw exception");
    } catch (Exception ignored) {}
  }

  @Test
  public void testImportNonExistingInnerNestedClass() {
    var code = """
        polyglot java import org.enso.example.TestClass.Non_Existing_Class.Another_Non_ExistingClass
        """;
    try {
      evalModule(ctx, code);
      fail("Should throw exception");
    } catch (Exception ignored) {}
  }

  @Test
  public void testImportOuterClassAndAccessNestedInnerClass() {
    var code = """
        polyglot java import org.enso.example.TestClass

        main =
            instance = TestClass.StaticInnerClass.StaticInnerInnerClass.new
            instance.mul 3 5
        """;
    var res = evalModule(ctx, code);
    assertEquals(15, res.asInt());
  }

  @Test
  public void testToStringBehavior() {
    var code = """
    from Standard.Base import all

    polyglot java import org.enso.example.ToString as Foo

    type My_Fooable_Implementation
        Instance x

        foo : Integer
        foo self = 100+self.x

    main =
        fooable = My_Fooable_Implementation.Instance 23
        a = fooable.foo
        b = fooable.to_text
        c = Foo.callFoo fooable
        d = Foo.showObject fooable
        e = Foo.callFooAndShow fooable
        [a, b, c, d, e]
    """;

    var res = evalModule(ctx, code);
    assertTrue("It is an array", res.hasArrayElements());
    assertEquals("Array with five elements", 5, res.getArraySize());
    assertEquals(123, res.getArrayElement(0).asInt());
    assertEquals("(Instance 23)", res.getArrayElement(1).asString());
    assertEquals("Fooable.foo() = 123", res.getArrayElement(2).asString());
    assertEquals("obj.toString() = (Instance 23)", res.getArrayElement(3).asString());
    assertEquals("{(Instance 23)}.foo() = 123", res.getArrayElement(4).asString());
  }

  @Test
  public void testInterfaceProxyFailuresA() {
    var payload = evalInterfaceProxyFailures("a");
    assertEquals("My_Exc", payload.getMetaObject().getMetaSimpleName());
    var stdout = getStdOutLines();
    var expectedLines = List.of("Executing Fooable_Panic.foo");
    assertArrayEquals(expectedLines.toArray(), stdout);
  }

  @Test
  public void testInterfaceProxyFailuresB() {
    var result = evalInterfaceProxyFailures("b");
    assertEquals("nonexistent_text_method", result.asString());
    var stdout = getStdOutLines();
    var expectedLines = List.of("Executing Fooable_Unresolved.foo");
    assertArrayEquals(expectedLines.toArray(), stdout);
  }

  private Value evalInterfaceProxyFailures(String methodToEval) {
    var code = """
        from Standard.Base import all
        import Standard.Base.Errors.Common.No_Such_Method

        polyglot java import org.enso.example.ToString as Foo

        type My_Exc
            Error

        type Fooable_Panic
            Value

            foo : Integer
            foo self =
                IO.println "Executing Fooable_Panic.foo"
                Panic.throw My_Exc.Error

        type Fooable_Unresolved
            Value

            foo : Integer
            foo self =
                IO.println "Executing Fooable_Unresolved.foo"
                "".nonexistent_text_method

        a = Panic.catch My_Exc (Foo.callFoo Fooable_Panic.Value) (.payload)
        b = Panic.catch No_Such_Method (Foo.callFoo Fooable_Unresolved.Value) (caught-> caught.payload.method_name)
        """;

    return evalModule(ctx, code + "\nmain = " + methodToEval);
  }
}
