package org.enso.refactoring.validation;

import org.junit.Assert;
import org.junit.Test;

public class SymbolNameValidationTest {


  public SymbolNameValidationTest() {}

  @Test
  public void isAllowedName() {
    Assert.assertFalse(MethodNameValidation.isAllowedName(""));
    Assert.assertFalse(MethodNameValidation.isAllowedName("_foo"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("42"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("42_foo"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("Foo"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("foo_Bar"));

    Assert.assertTrue(MethodNameValidation.isAllowedName("foo"));
    Assert.assertTrue(MethodNameValidation.isAllowedName("foo_bar"));
    Assert.assertTrue(MethodNameValidation.isAllowedName("foo_42"));
  }

  @Test
  public void normalizeName() {
    Assert.assertEquals("foo_42", MethodNameValidation.normalize("foo_42"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo_bar"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo__bar"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo$_$bar"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME, MethodNameValidation.normalize("!$%"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME, MethodNameValidation.normalize("!_%"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME + "_foo", MethodNameValidation.normalize("_foo"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo bar"));
    Assert.assertEquals("foo_42", MethodNameValidation.normalize("foo42"));
    Assert.assertEquals("foo_42_bar", MethodNameValidation.normalize("foo42bar"));
    Assert.assertEquals("foo_42_bar", MethodNameValidation.normalize("foo$ 42$bar"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("Foo Bar"));
    Assert.assertEquals("foobar", MethodNameValidation.normalize("FooBar"));
  }
}
