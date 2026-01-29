package org.enso.base.polyglot.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;

import java.time.LocalDate;
import java.util.Objects;
import java.util.Random;
import java.util.stream.IntStream;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.test.utils.ContextUtils;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class DateStorageTest {
  @ClassRule
  public static final ContextUtils ctx =
      ContextUtils.newBuilder("enso", "arrow").assertGC(false).build();

  @BeforeClass
  public static void importAll() {
    ctx.eval("enso", "from Standard.Base import all");
  }

  @Test
  public void makeLocalFromDateStorage() {
    var b = Builder.getForDate(3);
    var one = LocalDate.of(1973, 12, 10);
    var two = LocalDate.of(1975, 5, 3);
    b.append(one).appendNulls(1).append(two);
    var storage = b.seal();

    @SuppressWarnings("unchecked")
    var proxyStorage =
        (ColumnStorage<LocalDate>) BoolStorageTest.makeProxy(storage, ColumnStorage.class);
    var localStorage = Builder.makeLocal(proxyStorage);

    assertNotSame("local storage is a copy of storage", storage, localStorage);
    assertEquals("They have the same size", storage.getSize(), localStorage.getSize());
    assertEquals("They have the same type char", storage.typeChar(), localStorage.typeChar());
    assertEquals("They have the same type size", storage.typeSize(), localStorage.typeSize());
    for (var i = 0L; i < storage.getSize(); i++) {
      var elem = storage.getItemBoxed(i);
      var localElem = localStorage.getItemBoxed(i);
      assertEquals("At " + i, elem, localElem);
    }
  }

  @Test
  public void testCreateViaBuilderAndReadViaArrowSimple16() {
    generateAndCompare("Simple 16 values", 16, IntStream.range(0, 16));
  }

  @Test
  public void testCreateViaBuilderAndReadViaArrowRandom() {
    generateAndCompareWithSeed(System.currentTimeMillis());
  }

  private void generateAndCompareWithSeed(long seed) {
    var r = new Random(seed);
    var size = r.nextInt(256, 4096);
    var stream = r.ints(size, 0, 20000);
    var msg = "with seed " + seed + " size " + size;
    generateAndCompare(msg, size, stream);
  }

  private void generateAndCompare(String info, int size, IntStream r) {
    var sb = new StringBuilder();
    var b = Builder.getForDate(size);
    r.mapToObj(LocalDate::ofEpochDay).forEach(b::append);
    var storage = b.seal();
    assertEquals("Storage has the right size: " + storage, size, storage.getSize());
    assertNotEquals("Storage provides access to raw data", 0L, storage.addressOfData());
    assertNotEquals("Storage provides access to validity bitmap", 0L, storage.addressOfValidity());

    var arr =
        ctx.eval("arrow", "cast[Date32]")
            .execute(storage.addressOfData(), storage.getSize(), storage.addressOfValidity());
    for (var i = 0L; i < size; i++) {
      var elem0 = storage.getItemBoxed(i);
      var value1 = arr.getArrayElement(i);
      var elem1 = value1.isNull() ? null : value1.asDate();
      if (!Objects.equals(elem0, elem1)) {
        sb.append("\n  at ").append(i).append(" ").append(elem0).append(" != ").append(elem1);
      }
    }
    assertEquals(info + "\n" + sb.toString(), 0, sb.length());
  }
}
