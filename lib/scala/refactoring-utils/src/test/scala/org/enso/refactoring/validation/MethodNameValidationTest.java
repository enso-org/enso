package org.enso.refactoring.validation;

import org.junit.Assert;
import org.junit.Test;

public class MethodNameValidationTest {


  public MethodNameValidationTest() {}

  @Test
  public void isAllowedName() {
    Assert.assertFalse(MethodNameValidation.isAllowedName(""));
    Assert.assertFalse(MethodNameValidation.isAllowedName("@#$"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("_foo"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("42"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("42_foo"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("Foo"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("foo_Bar"));
    Assert.assertFalse(MethodNameValidation.isAllowedName("foo bar"));

    Assert.assertTrue(MethodNameValidation.isAllowedName("foo"));
    Assert.assertTrue(MethodNameValidation.isAllowedName("foo_bar"));
    Assert.assertTrue(MethodNameValidation.isAllowedName("foo_42"));
    Assert.assertTrue(MethodNameValidation.isAllowedName("foo42"));
  }

  @Test
  public void normalizeName() {
    Assert.assertEquals("foo", MethodNameValidation.normalize("FOO"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("FOO_BAR"));
    Assert.assertEquals("foo42_bar", MethodNameValidation.normalize("FOO42BAR"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("FOO bar"));
    Assert.assertEquals("f_oo_bar", MethodNameValidation.normalize("fOoBar"));
    Assert.assertEquals("foo_42", MethodNameValidation.normalize("foo_42"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo_bar"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo__bar"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo$_$bar"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME, MethodNameValidation.normalize("!$%"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME, MethodNameValidation.normalize("!_%"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME + "_foo", MethodNameValidation.normalize("_foo"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME + "_foo", MethodNameValidation.normalize("__foo"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME + "_foo", MethodNameValidation.normalize("__foo__"));
    Assert.assertEquals(MethodNameValidation.DEFAULT_NAME + "_foo", MethodNameValidation.normalize("  foo  "));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("foo bar"));
    Assert.assertEquals("foo42", MethodNameValidation.normalize("foo42"));
    Assert.assertEquals("foo42_bar", MethodNameValidation.normalize("foo42bar"));
    Assert.assertEquals("foo_42_bar", MethodNameValidation.normalize("foo$ 42$bar"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("fooBar"));
    Assert.assertEquals("foo_bar", MethodNameValidation.normalize("FooBar"));
    Assert.assertEquals("foo42_bar", MethodNameValidation.normalize("Foo42Bar"));
  }
}
