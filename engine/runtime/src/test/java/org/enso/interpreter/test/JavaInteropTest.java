package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.AfterClass;
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

  private void checkPrint(String code, List<String> expected) {
    Value result = evalModule(ctx, code);
    assertTrue("should return Nothing", result.isNull());
    String[] logLines = out
        .toString(StandardCharsets.UTF_8)
        .trim()
        .split(System.lineSeparator());
    assertArrayEquals(expected.toArray(), logLines);
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
}
