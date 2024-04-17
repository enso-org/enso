package org.enso.interpreter.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class PrivateAccessTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void initContext() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void closeContext() {
    ctx.close();
  }

  @Test
  public void privateConstructorCannotBeAccessedFromDifferentProject() {
    try {
      var res = evalTestProject(defaultContextBuilder(), "Test_Private_Access_2");
      fail("Should throw PolyglotException");
    } catch (PolyglotException e) {
      // TODO: check the exception message
      System.out.println("PolyglotException: " + e);
    }
  }

  /**
   * Tests that the private ctor can be accessed from the same project via a public getter method.
   */
  @Test
  public void privateCtorCanBeAccessedFromSameProject() {
    var res = evalTestProject(defaultContextBuilder(), "Test_Private_Access_3");
    assertThat(res.isNumber(), is(true));
    assertThat(res.asInt(), is(42));
  }

  /** Tests that the private field `f1` cannot be accessed from a different project. */
  @Test
  public void privateGetterCannotBeAccessedFromDifferentProject1() {
    try {
      var res = evalTestProject(defaultContextBuilder(), "Test_Private_Access_4");
      fail("Should throw PolyglotException");
    } catch (PolyglotException e) {
      // TODO: check the exception message
      System.out.println("PolyglotException: " + e);
    }
  }

  /** Tests that the private field `f2` cannot be accessed from a different project. */
  @Test
  public void privateGetterCannotBeAccessedFromDifferentProject2() {
    try {
      var res = evalTestProject(defaultContextBuilder(), "Test_Private_Access_5");
      fail("Should throw PolyglotException");
    } catch (PolyglotException e) {
      // TODO: check the exception message
      System.out.println("PolyglotException: " + e);
    }
  }

  /**
   * Tests that the private constructor can be passed to a different project as a lambda method, and
   * ca be called there, in the different project.
   */
  @Test
  public void privateCtorCanBeCalledAsCallback() {
    var res = evalTestProject(defaultContextBuilder(), "Test_Private_Access_6");
    assertThat(res.isNumber(), is(true));
    assertThat(res.asInt(), is(42));
  }

  @Test
  public void privateFieldCanBeAccessedViaPublicWrapper() {
    var res = evalTestProject(defaultContextBuilder(), "Test_Private_Access_7");
    assertThat(res.isNumber(), is(true));
    assertThat(res.asInt(), is(42));
  }
}
