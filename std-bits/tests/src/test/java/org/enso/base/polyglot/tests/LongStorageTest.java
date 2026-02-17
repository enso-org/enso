package org.enso.base.polyglot.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;

import java.util.Objects;
import java.util.Random;
import java.util.stream.LongStream;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.test.utils.ContextUtils;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class LongStorageTest {
  @ClassRule
  public static final ContextUtils ctx =
      ContextUtils.newBuilder("enso", "arrow").assertGC(false).build();

  @BeforeClass
  public static void importAll() {
    ctx.eval("enso", "from Standard.Base import all");
  }

  @Test
  public void makeLocalFromLongStorage() {
    var b = Builder.getForLong(IntegerType.INT_64, 3, problemAggregator());
    b.append(1).appendNulls(1).append(2);
    var storage = b.seal();
    var localStorage = Builder.makeLocal(storage);
    assertNotSame("local storage is a copy of storage", storage, localStorage);
    assertEquals(
        "They have data at the same address",
        storage.addressOfData(),
        localStorage.addressOfData());
    assertEquals("They have the same size", storage.getSize(), localStorage.getSize());
    assertEquals("They have the same type", storage.getType(), localStorage.getType());
    for (var i = 0L; i < storage.getSize(); i++) {
      var elem = storage.getItemBoxed(i);
      var localElem = localStorage.getItemBoxed(i);
      assertEquals("At " + i, elem, localElem);
    }
  }

  @Test
  public void makeLocalFromSmallLongStorage() {
    var b = Builder.getForLong(IntegerType.INT_32, 3, problemAggregator());
    b.append(1).appendNulls(1).append(2);
    var storage = b.seal();
    var localStorage = Builder.makeLocal(storage);
    assertNotSame("local storage is a copy of storage", storage, localStorage);
    assertEquals(
        "They have data at the same address",
        storage.addressOfData(),
        localStorage.addressOfData());
    assertEquals("They have the same size", storage.getSize(), localStorage.getSize());
    assertEquals("They have the same type", storage.getType(), localStorage.getType());
    for (var i = 0L; i < storage.getSize(); i++) {
      var elem = storage.getItemBoxed(i);
      var localElem = localStorage.getItemBoxed(i);
      assertEquals("At " + i, elem, localElem);
    }
  }

  @Test
  public void testCreateViaBuilderAndReadViaArrowSimple16() {
    generateAndCompare("Simple 16 values", 16, LongStream.range(0, 16));
  }

  @Test
  public void testCreateViaBuilderAndReadViaArrowRandom() {
    generateAndCompareWithSeed(System.currentTimeMillis());
  }

  private void generateAndCompareWithSeed(long seed) {
    var r = new Random(seed);
    var size = r.nextInt(256, 4096);
    var stream = r.longs(size);
    var msg = "with seed " + seed + " size " + size;
    System.err.println(msg);
    generateAndCompare(msg, size, stream);
  }

  private void generateAndCompare(String info, int size, LongStream r) {
    var sb = new StringBuilder();
    var b = Builder.getForLong(IntegerType.INT_64, size, null);
    r.forEach(b::append);
    var storage = b.seal();
    assertEquals("Storage has the right size: " + storage, size, storage.getSize());
    assertNotEquals("Storage provides acccess to raw data", 0L, storage.addressOfData());
    assertNotEquals("Storage provides access to validity bitmap", 0L, storage.addressOfValidity());

    var arr =
        ctx.eval("arrow", "cast[Int64]")
            .execute(storage.addressOfData(), storage.getSize(), storage.addressOfValidity());
    for (var i = 0L; i < size; i++) {
      var elem0 = storage.getItemBoxed(i);
      var value1 = arr.getArrayElement(i);
      var elem1 = value1.isNull() ? null : value1.asLong();
      if (!Objects.equals(elem0, elem1)) {
        sb.append("\n  at ").append(i).append(" ").append(elem0).append(" != ").append(elem1);
      }
    }
    assertEquals(info + "\n" + sb.toString(), 0, sb.length());
  }

  private static ProblemAggregator problemAggregator() {
    return BlackholeProblemAggregator.INSTANCE;
  }
}
