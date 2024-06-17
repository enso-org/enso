package org.enso.interpreter.arrow;

import static org.junit.Assert.*;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class AddArrowTest {
  private static Context ctx;

  @BeforeClass
  public static void initEnsoContext() {
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .out(System.out)
            .err(System.err)
            .allowAllAccess(true)
            .build();
  }

  @AfterClass
  public static void closeEnsoContext() throws Exception {
    if (ctx != null) {
      ctx.close();
    }
  }

  @Test
  public void addTwoInt8ArrowArrays() {
    var arrow = ctx.getEngine().getLanguages().get("arrow");
    assertNotNull("Arrow is available", arrow);
    var int8Constr = ctx.eval("arrow", "new[Int8]");
    assertNotNull(int8Constr);

    var arrLength = 10;
    var builder1 = int8Constr.newInstance(arrLength);
    var builder2 = int8Constr.newInstance(arrLength);

    for (var i = 0; i < arrLength; i++) {
      var ni = arrLength - i - 1;
      var v = i * i;
      builder1.invokeMember("append", i, (byte) v);
      builder2.invokeMember("append", ni, (byte) v);
    }

    var arr1 = builder1.invokeMember("build");
    assertEquals("Right size of arr1", arrLength, arr1.getArraySize());
    var arr2 = builder2.invokeMember("build");
    assertEquals("Right size of arr2", arrLength, arr2.getArraySize());

    var int8Plus = ctx.eval("arrow", "+[Int8]");
    var resultArr = int8Plus.execute(arr1, arr2);

    assertTrue("Result is an array", resultArr.hasArrayElements());
    assertEquals("Right size", arrLength, resultArr.getArraySize());

    for (var i = 0; i < arrLength; i++) {
      var ni = arrLength - i - 1;
      var v1 = resultArr.getArrayElement(i).asLong();
      var v2 = resultArr.getArrayElement(ni).asLong();

      assertEquals("Values at " + i + " and " + ni + " are the same", v1, v2);
      assertTrue("Values are always bigger than zero: " + v1, v1 > 0);
    }
  }

  @Test
  public void addTwoInt64ArrowArraysWithNulls() {
    var arrow = ctx.getEngine().getLanguages().get("arrow");
    assertNotNull("Arrow is available", arrow);
    var constr = ctx.eval("arrow", "new[Int64]");
    assertNotNull(constr);

    var arrLength = 10;
    var builder1 = constr.newInstance(arrLength);
    for (int i = 0; i < arrLength; i++) {
      if (i % 7 < 2) {
        builder1.invokeMember("append", Long.MAX_VALUE);
      } else {
        builder1.invokeMember("append", i);
      }
    }

    var builder2 = constr.newInstance(arrLength);
    for (var i = 0; i < arrLength; i++) {
      builder2.invokeMember("append", 10 + i);
    }

    var arr1 = builder1.invokeMember("build");
    assertEquals("Right size of arr1", arrLength, arr1.getArraySize());
    var addArr = builder2.invokeMember("build");
    assertEquals("Right size of arr2", arrLength, addArr.getArraySize());

    var plus = ctx.eval("arrow", "+[Int64]");
    var res1 = plus.execute(arr1, addArr);

    assertTrue("Result is an array", res1.hasArrayElements());
    assertEquals("Right size", arrLength, res1.getArraySize());

    assertTrue("is null", res1.getArrayElement(0).isNull());
    assertTrue("is null", res1.getArrayElement(1).isNull());
    assertTrue("is null", res1.getArrayElement(7).isNull());
    assertTrue("is null", res1.getArrayElement(8).isNull());

    var countNulls = 0;
    for (var i = 0; i < arrLength; i++) {
      var v = res1.getArrayElement(i);
      if (v.isNull()) {
        countNulls++;
      } else {
        assertEquals(i * 2 + 10, v.asLong());
      }
    }
    assertEquals("Four nulls", 4, countNulls);

    var res2 = plus.execute(res1, addArr);

    assertTrue("Result is an array", res2.hasArrayElements());
    assertEquals("Right size", arrLength, res2.getArraySize());

    assertTrue("is null", res2.getArrayElement(0).isNull());
    assertTrue("is null", res2.getArrayElement(1).isNull());
    assertTrue("is null", res2.getArrayElement(7).isNull());
    assertTrue("is null", res2.getArrayElement(8).isNull());

    var countNullsAgain = 0;
    for (var i = 0; i < arrLength; i++) {
      var v = res2.getArrayElement(i);
      if (v.isNull()) {
        countNullsAgain++;
      } else {
        assertEquals(i * 3 + 20, v.asLong());
      }
    }
    assertEquals("Four nulls", 4, countNullsAgain);
  }
}
