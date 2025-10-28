package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import org.enso.example.TestClass;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.Test;

/**
 * Tests {@code polyglot java import} behavior in isolation. When there is a problem with
 * interactions with the Java classes, it is best to expand this test. It is easier to debug the
 * problem then having whole integration tests and moreover this suite executes the same test in
 * various configurations automatically.
 *
 * <p>The test itself is abstract class and just defines the test cases. Then there are various
 * implementations:
 *
 * <ul>
 *   <li>{@link HostJavaInteropTest} - regular GraalVM <em>host interop</em> with JVM classes
 *   <li>{@link GuestJavaInteropTest} - dual JVM mode used when running Enso in <em>native
 *       image</em> mode and loading classes in separate <b>HotSpot</b> JVM
 * </ul>
 *
 * Those implementations setup the {@link #ctx()} and execute the test in that setup. This way we
 * can guarantee consistency between various implementations of the {@code polyglot java import}
 * statements.
 *
 * <p>Execute all these tests as:
 *
 * <pre>
 * sbt:enso> runtime-integration-tests/testOnly *JavaInteropTest
 * </pre>
 */
public abstract class JavaInteropTest {

  @After
  public void resetOutput() {
    ctx().resetOut();
  }

  private String[] getStdOutLines() {
    return ctx().getOut().trim().split(System.lineSeparator());
  }

  private void checkPrint(String code, List<String> expected) {
    Value result = ctx().evalModule(code);
    assertTrue("should return Nothing", result.isNull());
    assertArrayEquals(expected.toArray(), getStdOutLines());
  }

  @Test
  public void testClassImport() {
    var code =
        """
        polyglot java import org.enso.example.TestClass
        main = TestClass.add 1 2
        """;
    var result = ctx().evalModule(code);
    assertEquals(3, result.asInt());
  }

  @Test
  public void testClassImportAndMethodCall() {
    var code =
        """
        import Standard.Base.Data.Numbers
        polyglot java import org.enso.example.TestClass
        main =
            instance = TestClass.new (x -> x * 2)
            instance.callFunctionAndIncrement 10
        """;
    var result = ctx().evalModule(code);
    assertEquals(21, result.asInt());
  }

  @Test
  public void testImportStaticInnerClass() {
    var code =
        """
        polyglot java import org.enso.example.TestClass.StaticInnerClass

        main =
            instance = StaticInnerClass.new "my_data"
            instance.add 1 2
        """;
    var result = ctx().evalModule(code);
    assertEquals(3, result.asInt());
  }

  @Test
  public void testImportInnerEnum() {
    var code =
        """
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
    var code =
        """
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
  public void testCaseOnFunctionalInterface() {
    var code =
        """
        polyglot java import org.enso.example.TestClass
        polyglot java import org.enso.example.TestClass.FnIntrfc

        check x y=42 = case x of
          call:FnIntrfc -> call.perform y
          "alien" -> TestClass.alien
          "real" -> TestClass.real
          "subclass" -> TestClass.subclass
          _ -> "no"

        main = check
        """;
    var check = ctx().evalModule(code);

    assertEquals("'no'", check.execute("Not FnIntrfc").toString());

    var alien = check.execute("alien"); // Function<Object, Object> alien = (x) -> x;

    assertEquals(
        "Function isn't the right Java interface", "'no'", check.execute(alien).toString());

    var real = check.execute("real"); // TestClass.FnIntrfc real = (x) -> x;
    assertEquals(
        "FnIntrfc is the right interface", "'good'", check.execute(real, "good").toString());

    var atomCode =
        """
        type My_Type
            Value x

        main = My_Type.Value 1
        """;
    var atom = ctx().evalModule(atomCode);
    assertEquals(
        "atom is not Java interface at all " + "and it shouldn't pass the call:FnIntrfc check",
        "'no'",
        check.execute(atom).toString());

    var subclass = check.execute("subclass"); // new TestClass.FnIntrfcSubclass();
    assertEquals(
        "FnIntrfcSubclass implements the right interface",
        "'subclass'",
        check.execute(subclass, "good").toString());

    assertEquals(
        "TestClass doesn't implement the interface",
        "'no'",
        check.execute(new TestClass(), "good").toString());
  }

  @Test
  public void testCaseNonFinal() {
    var code =
        """
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
    var code =
        """
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
    var code =
        """
        polyglot java import org.enso.example.TestClass

        main =
            instance = TestClass.StaticInnerClass.new "my_data"
            instance.getData
        """;
    var result = ctx().evalModule(code);
    assertEquals("my_data", result.asString());
  }

  @Test
  public void testImportBothInnerAndOuterClass() {
    var code =
        """
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
    var code =
        """
        polyglot java import org.enso.example.TestClass.StaticInnerClass.StaticInnerInnerClass

        main =
            inner_inner_value = StaticInnerInnerClass.new
            inner_inner_value.mul 3 5
        """;
    var res = ctx().evalModule(code);
    assertEquals(15, res.asInt());
  }

  @Test
  public void testImportNonExistingInnerClass() {
    var code =
        """
        polyglot java import org.enso.example.TestClass.StaticInnerClass.Non_Existing_Class
        """;
    try {
      ctx().evalModule(code);
      fail("Should throw exception");
    } catch (Exception ignored) {
    }
  }

  @Test
  public void testImportNonExistingInnerNestedClass() {
    var code =
        """
        polyglot java import org.enso.example.TestClass.Non_Existing_Class.Another_Non_ExistingClass
        """;
    try {
      ctx().evalModule(code);
      fail("Should throw exception");
    } catch (Exception ignored) {
    }
  }

  @Test
  public void testImportOuterClassAndAccessNestedInnerClass() {
    var code =
        """
        polyglot java import org.enso.example.TestClass

        main =
            instance = TestClass.StaticInnerClass.StaticInnerInnerClass.new
            instance.mul 3 5
        """;
    var res = ctx().evalModule(code);
    assertEquals(15, res.asInt());
  }

  @Test
  public void testToStringBehavior() {
    var code =
        """
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

    var res = ctx().evalModule(code);
    assertTrue("It is an array", res.hasArrayElements());
    assertEquals("Array with five elements", 5, res.getArraySize());
    assertEquals(123, res.getArrayElement(0).asInt());
    assertEquals("(Instance 23)", res.getArrayElement(1).asString());
    assertEquals("Fooable.foo() = 123", res.getArrayElement(2).asString());
    assertEquals("obj.toString() = (Instance 23)", res.getArrayElement(3).asString());
    assertEquals("{(Instance 23)}.foo() = 123", res.getArrayElement(4).asString());
  }

  @Test
  public void testToStringBehaviorSimple1() {
    var code =
        """
        from Standard.Base import all

        polyglot java import org.enso.example.ToString as Foo

        type My_Fooable_Implementation
            Instance x

            foo : Integer
            foo self = 100+self.x

        main =
            fooable = My_Fooable_Implementation.Instance 23
            e = Foo.callFooAndShow fooable
            e
        """;

    var res = ctx().evalModule(code);
    assertEquals("{(Instance 23)}.foo() = 123", res.asString());
  }

  @Test
  public void throwsParsingError() {
    var code =
        """
        from Standard.Base import Panic
        polyglot java import java.lang.Integer as Num
        polyglot java import java.lang.NumberFormatException as Ex

        main =
          Panic.catch Ex (Num.parseInt "NotAnInt") .payload
        """;

    var res = ctx().evalModule(code);
    assertTrue("Got an exception back", res.isException());
    var typeEx = res.getMetaObject();
    assertEquals("java.lang.NumberFormatException", typeEx.getMetaQualifiedName());
    try {
      throw res.throwException();
    } catch (PolyglotException ex) {
      assertEquals("For input string: \"NotAnInt\"", ex.getMessage());
    }
  }

  @Test
  public void throwsParsingErrorIndirect() {
    var code =
        """
        from Standard.Base import Panic
        polyglot java import java.lang.Integer as Num
        polyglot java import java.lang.NumberFormatException as Ex
        polyglot java import org.enso.example.TestClass

        type En
          Err msg

        main =
          e = TestClass.newDirectExecutor
          e.execute
              Panic.catch Ex (Num.parseInt "NotAnInt") ex->
                  Panic.throw (En.Err ex.payload.to_text)
        """;

    try {
      var res = ctx().evalModule(code);
      fail("Expecting an exception: " + res);
    } catch (PolyglotException ex) {
      var exObj = ex.getGuestObject();
      var typeEx = exObj.getMetaObject();
      assertEquals("Standard.Base.Panic.Panic", typeEx.getMetaQualifiedName());
      assertEquals(
          "java.lang.NumberFormatException: For input string: \"NotAnInt\"",
          exObj.getMember("msg").asString());
    }
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
    var code =
        """
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

    return ctx().evalModule(code + "\nmain = " + methodToEval);
  }

  @Test
  public void catchCheckedExceptionValueIsReturned() {
    var result = checkedException(0);
    assertEquals(result.asInt(), 10);
  }

  @Test
  public void catchCheckedExceptionThrownInEnso() {
    var result = checkedException(1);
    assertEquals(result.asInt(), -1);
  }

  @Test
  public void catchCheckedExceptionThrownInJava() {
    var result = checkedException(2);
    assertEquals(result.asInt(), -1);
  }

  @Test
  public void catchCheckedSubExceptionThrownInJava() {
    var result = checkedException(3);
    assertEquals(result.asInt(), -1);
  }

  private Value checkedException(int t) {
    var code =
        """
        polyglot java import org.enso.example.TestException
        from Standard.Base import Panic

        handle_errors ~action  =
            Panic.catch TestException action caught_panic->
              -1

        run t = case t of
          0 -> handle_errors 10
          1 -> handle_errors (Panic.throw TestException.new)
          2 -> handle_errors (TestException.throwMe)
          3 -> handle_errors (TestException.throwSubtype)

        main = run
        """;
    var result = ctx().evalModule(code);
    return result.execute(t);
  }

  protected abstract ContextUtils ctx();
}
