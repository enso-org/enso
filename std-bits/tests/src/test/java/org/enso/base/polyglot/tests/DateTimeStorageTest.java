package org.enso.base.polyglot.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.test.utils.ContextUtils;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class DateTimeStorageTest {
  @ClassRule
  public static final ContextUtils ctx = ContextUtils.newBuilder("enso").assertGC(false).build();

  @BeforeClass
  public static void importAll() {
    ctx.eval("enso", "from Standard.Base import all");
  }

  @Test
  public void makeLocalFromDateTimeStorage() {
    var b = Builder.getForDateTime(3);
    var one = LocalDateTime.of(2005, 1, 10, 12, 30);
    var two = LocalDateTime.of(2006, 8, 23, 6, 13);
    b.append(one).appendNulls(1).append(two);
    var storage = b.seal();

    @SuppressWarnings("unchecked")
    var proxyStorage =
        (ColumnStorage<ZonedDateTime>) BoolStorageTest.makeProxy(storage, ColumnStorage.class);
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
  public void makeZonedFromDateTimeStorage() {
    var b = Builder.getForDateTime(3);
    var one = ZonedDateTime.of(LocalDateTime.of(2005, 1, 10, 12, 30), ZoneOffset.of("+01:00"));
    var two = ZonedDateTime.of(LocalDateTime.of(2006, 8, 23, 6, 13), ZoneOffset.of("+02:00"));
    b.append(one).appendNulls(1).append(two);
    var storage = b.seal();

    @SuppressWarnings("unchecked")
    var proxyStorage =
        (ColumnStorage<ZonedDateTime>) BoolStorageTest.makeProxy(storage, ColumnStorage.class);
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
  public void makeZonedFromDateTimeStorageWithNanos() {
    var b = Builder.getForDateTime(3);
    var one =
        ZonedDateTime.of(
            LocalDateTime.of(2005, 1, 10, 12, 30, 43, 691234), ZoneOffset.of("+01:00"));
    var two =
        ZonedDateTime.of(LocalDateTime.of(2006, 8, 23, 6, 13, 1, 901430), ZoneOffset.of("+02:00"));
    var three = ZonedDateTime.of(LocalDateTime.of(2020, 10, 23, 23, 2, 3, 4005), ZoneId.of("UTC"));
    b.append(one).appendNulls(1).append(two).append(three);
    var storage = b.seal();

    @SuppressWarnings("unchecked")
    var proxyStorage =
        (ColumnStorage<ZonedDateTime>) BoolStorageTest.makeProxy(storage, ColumnStorage.class);
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
}
