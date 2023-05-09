package org.enso.interpreter.test;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.After;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class PolyglotErrorTest extends TestBase {
  private Context ctx;
  private Value panic;

  public static String bar(Object o) {
    return "[[" + o + "]]";
  }

  @Before
  public void prepareCtx() throws Exception {
    this.ctx = createDefaultContext();

    var code = """
    import Standard.Base.Panic.Panic
    import Standard.Base.Data.Text.Text
    import Standard.Base.Error.Error
    import Standard.Base.Errors.Illegal_State.Illegal_State
    import Standard.Base.Nothing.Nothing
    import Standard.Base.Warning.Warning
    polyglot java import org.enso.interpreter.test.PolyglotErrorTest

    type TypeCa
        Ca x

        to_text : Text
        to_text self = "<<Ca "+self.x.to_text+">>"

    type TypeCb
        Cb x

        to_text : Text
        to_text self = Error.throw (Illegal_State.Error "B")

    type TypeCc
        Cc x

        to_text : Text
        to_text self = Panic.throw (Illegal_State.Error "C")

    type TypeCd
        Cd x

        to_text self = 42

    type TypeCe
        Ce x

        to_text self = Warning.attach "Some random warning" self.x

    panic x = case x of
        1 -> panic1
        2 -> panic2
        3 -> panic3
        4 -> panic4
        5 -> panic5
        _ -> panic6

    panic1 = PolyglotErrorTest.bar (TypeCa.Ca 'x')

    panic2 =
        PolyglotErrorTest.bar (TypeCb.Cb 'y') . catch err->
            "{Error: "+err.to_text+"}"

    panic3 =
        Panic.catch Illegal_State (PolyglotErrorTest.bar (TypeCc.Cc 'z')) caught_panic->
            "{Panic: "+caught_panic.payload.to_text+"}"

    panic4 = PolyglotErrorTest.bar (TypeCd.Cd Nothing)

    panic5 = PolyglotErrorTest.bar (TypeCe.Ce "Foo")
    panic6 = PolyglotErrorTest.bar (TypeCe.Ce 44)
    """;
    var src = Source.newBuilder("enso", code, "test.enso").build();
    var module = ctx.eval(src);

    this.panic = module.invokeMember("eval_expression", "panic");
    assertTrue("It is a function", this.panic.canExecute());
  }

  @After
  public void disposeCtx() {
    this.ctx.close();
  }

  @Test
  public void panic1() {
    var v = panic.execute(1);
    assertTrue("Is string", v.isString());
    assertEquals("[[<<Ca x>>]]", v.asString());
  }

  @Test
  public void panic2() {
    var v = panic.execute(2);
    assertTrue("Is string", v.isString());
    assertEquals("[[Error in method `to_text` of [Cb y]: (Error: (Illegal_State.Error 'B' Nothing))]]", v.asString());
  }

  @Test
  public void panic3() {
    var v = panic.execute(3);
    assertTrue("Is string", v.isString());
    assertEquals("[[Panic in method `to_text` of [Cc z]: (Illegal_State.Error 'C' Nothing)]]", v.asString());
  }

  @Test
  public void panic4() {
    var v = panic.execute(4);
    assertTrue("Is string", v.isString());
    assertEquals("[[Error in method `to_text` of [Cd Nothing]: Expected Text but got 42]]", v.asString());
  }

  @Test
  public void panic5() {
    var v = panic.execute(5);
    assertTrue("Is string", v.isString());
    assertEquals("[[Foo]]", v.asString());
  }

  @Test
  public void panic6() {
    var v = panic.execute(6);
    assertTrue("Is string", v.isString());
    assertEquals("[[Error in method `to_text` of [Ce 44]: Expected Text but got 44]]", v.asString());
  }
}
