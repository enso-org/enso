package org.enso.interpreter.arrow;

import static org.junit.Assert.*;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;
import org.apache.arrow.memory.BufferAllocator;
import org.apache.arrow.memory.RootAllocator;
import org.apache.arrow.vector.BaseFixedWidthVector;
import org.apache.arrow.vector.BigIntVector;
import org.apache.arrow.vector.IntVector;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class VerifyArrowTest {
  private static Context ctx;
  private static Handler handler;

  @BeforeClass
  public static void initEnsoContext() {
    handler = new MockHandler();
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .logHandler(handler)
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
  public void arrowDate32() {
    var arrow = ctx.getEngine().getLanguages().get("arrow");
    assertNotNull("Arrow is available", arrow);
    var date32Constr = ctx.eval("arrow", "new[Date32]");

    Value date32Array = date32Constr.newInstance(10);
    assertNotNull("allocated value should not be null", date32Array);
    assertTrue("allocated value should be an array", date32Array.hasArrayElements());
    var startDate = LocalDate.now();
    populateArrayWithConsecutiveDays(date32Array, startDate);
    var rawDayPlus2 = date32Array.getArrayElement(2);
    var dayPlus2 = rawDayPlus2.asDate();
    assertFalse(rawDayPlus2.isTime() || rawDayPlus2.isTimeZone());
    assertEquals(startDate.plusDays(2), dayPlus2);

    var startDateTime = ZonedDateTime.now();
    populateArrayWithConsecutiveDays(date32Array, startDateTime);
    rawDayPlus2 = date32Array.getArrayElement(2);
    assertFalse(rawDayPlus2.isTime());
  }

  @Test
  public void arrowDate64() {
    var arrow = ctx.getEngine().getLanguages().get("arrow");
    assertNotNull("Arrow is available", arrow);
    var date64Constr = ctx.eval("arrow", "new[Date64]");

    Value date64Array = date64Constr.newInstance(10);
    assertNotNull("allocated value should not be null", date64Array);
    assertTrue("allocated value should be an array", date64Array.hasArrayElements());
    var startDate = ZonedDateTime.now(ZoneId.of("Europe/Paris"));
    var startDateZone = startDate.getZone();
    populateArrayWithConsecutiveDays(date64Array, startDate);
    var rawZonedDateTime = date64Array.getArrayElement(2);
    var dayPlus2 =
        rawZonedDateTime.asDate().atTime(rawZonedDateTime.asTime()).atZone(startDateZone);
    var startDateInstant = startDate.toInstant().atZone(startDate.getZone());
    assertTrue(startDateInstant.plusDays(2).isEqual(dayPlus2));
    assertFalse(startDate.isEqual(dayPlus2));

    var startDate2 = ZonedDateTime.parse("2023-11-01T02:00:01+01:00[Europe/Paris]");
    var startDate2Zone = startDate2.getZone();
    var startDate2Pnf = ZonedDateTime.parse("2023-11-01T02:00:01-07:00[US/Pacific]");
    populateArrayWithConsecutiveDays(date64Array, startDate2);
    rawZonedDateTime = date64Array.getArrayElement(2);
    dayPlus2 = rawZonedDateTime.asDate().atTime(rawZonedDateTime.asTime()).atZone(startDate2Zone);
    assertTrue(startDate2.plusDays(2).isEqual(dayPlus2));
    assertFalse(startDate2Pnf.plusDays(2).isEqual(dayPlus2));

    date64Array.setArrayElement(5, null);
    date64Array.setArrayElement(9, null);
    assertFalse(date64Array.getArrayElement(4).isNull());
    assertTrue(date64Array.getArrayElement(5).isNull());
    assertTrue(date64Array.getArrayElement(9).isNull());
  }

  @Test
  public void arrowInt8() {
    var arrow = ctx.getEngine().getLanguages().get("arrow");
    assertNotNull("Arrow is available", arrow);
    var int8Constr = ctx.eval("arrow", "new[Int8]");
    assertNotNull(int8Constr);

    Value int8Array = int8Constr.newInstance(10);
    assertNotNull(int8Array);
    populateIntArray(int8Array, (byte) 42);
    var v = int8Array.getArrayElement(5);
    assertEquals((byte) 47, v.asByte());
    int8Array.setArrayElement(5, 21);
    v = int8Array.getArrayElement(5);
    assertEquals((byte) 21, v.asByte());
    try {
      int8Array.setArrayElement(5, 300);
      fail("expected out of bounds exception");
    } catch (UnsupportedOperationException e) {
    }
  }

  @Test
  public void castInt() {
    var typeLength = LogicalLayout.Int32;
    var testValues = new Object[] {3, 1, 5, 3};
    try (BufferAllocator allocator = new RootAllocator();
        BaseFixedWidthVector intVector =
            allocateFixedLengthVector(allocator, testValues, typeLength); ) {
      var int32Constr = ctx.eval("arrow", "cast[" + typeLength + "]");
      assertNotNull(int32Constr);
      Value int32Array =
          int32Constr.execute(
              intVector.getDataBufferAddress(),
              intVector.getDataBuffer().capacity() / typeLength.sizeInBytes());

      assertNotNull(int32Array);
      for (int i = 0; i < testValues.length; i++) {
        if (testValues[i] != null) {
          assertEquals(testValues[i], int32Array.getArrayElement(i).asInt());
        } else {
          assertTrue(int32Array.getArrayElement(i).isNull());
        }
      }
    }

    testValues = new Object[] {3, null, 5, 3, 7, 18, null, 9, 7, null, null, 100};
    try (BufferAllocator allocator = new RootAllocator();
        BaseFixedWidthVector intVector =
            allocateFixedLengthVector(allocator, testValues, typeLength); ) {
      var int32Constr = ctx.eval("arrow", "cast[" + typeLength + "]");
      assertNotNull(int32Constr);
      Value int32Array =
          int32Constr.execute(
              intVector.getDataBufferAddress(),
              intVector.getDataBuffer().capacity() / typeLength.sizeInBytes(),
              intVector.getValidityBufferAddress());

      assertNotNull(int32Array);
      for (int i = 0; i < testValues.length; i++) {
        if (testValues[i] != null) {
          assertEquals(testValues[i], int32Array.getArrayElement(i).asInt());
        } else {
          assertTrue(int32Array.getArrayElement(i).isNull());
        }
      }

      // Verify vector is memory-mapped, not copied
      assertTrue(int32Array.getArrayElement(10).isNull());
      ((IntVector) intVector).set(10, 12);
      assertFalse(int32Array.getArrayElement(10).isNull());
      assertEquals(12, int32Array.getArrayElement(10).asInt());
    }

    typeLength = LogicalLayout.Int64;
    testValues = new Object[] {(long) 3, null, (long) 5, (long) 3};
    try (BufferAllocator allocator = new RootAllocator();
        BaseFixedWidthVector vector =
            allocateFixedLengthVector(allocator, testValues, typeLength); ) {
      var int32Constr = ctx.eval("arrow", "cast[" + typeLength + "]");
      assertNotNull(int32Constr);
      Value int32Array =
          int32Constr.execute(
              vector.getDataBufferAddress(),
              vector.getDataBuffer().capacity() / typeLength.sizeInBytes(),
              vector.getValidityBufferAddress());

      assertNotNull(int32Array);
      for (int i = 0; i < testValues.length; i++) {
        if (testValues[i] != null) {
          assertEquals(testValues[i], int32Array.getArrayElement(i).asLong());
        } else {
          assertTrue(int32Array.getArrayElement(i).isNull());
        }
      }
    }
  }

  private BaseFixedWidthVector allocateFixedLengthVector(
      BufferAllocator allocator, Object[] testValues, LogicalLayout unit) {
    var valueCount = 0;
    switch (unit) {
      case Int32:
        var intVector = new IntVector("fixed-size-primitive-layout", allocator);
        intVector.allocateNew(testValues.length);
        for (int i = 0; i < testValues.length; i++) {
          if (testValues[i] != null) {
            intVector.set(i, (int) testValues[i]);
            valueCount++;
          } else {
            intVector.setNull(i);
          }
        }
        intVector.setValueCount(valueCount);
        return intVector;
      case Int64:
        var bigIntVector = new BigIntVector("fixed-size-primitive-layout", allocator);
        bigIntVector.allocateNew(testValues.length);
        for (int i = 0; i < testValues.length; i++) {
          if (testValues[i] != null) {
            bigIntVector.set(i, (long) testValues[i]);
            valueCount++;
          } else {
            bigIntVector.setNull(i);
          }
        }
        bigIntVector.setValueCount(valueCount);
        return bigIntVector;
      default:
        throw new RuntimeException("unable to create a vector for " + unit);
    }
  }

  private void populateArrayWithConsecutiveDays(Value arr, Temporal startDate) {
    var len = arr.getArraySize();
    for (int i = 0; i < len; i++) {
      arr.setArrayElement(i, startDate.plus(2, java.time.temporal.ChronoUnit.DAYS));
    }
  }

  private void populateIntArray(Value arr, byte startValue) {
    var len = arr.getArraySize();
    for (int i = 0; i < len; i++) {
      var v = startValue + i;
      arr.setArrayElement(i, (byte) v);
    }
  }

  private static class MockHandler extends Handler {
    private final Formatter fmt = new SimpleFormatter();
    private final List<LogRecord> records = new ArrayList<>();
    private String failMsg;
    private Error failure;

    public MockHandler() {}

    public void failOnMessage(String msg) {
      this.failMsg = msg;
    }

    @Override
    public void publish(LogRecord lr) {
      records.add(lr);
      var msg = fmt.formatMessage(lr);
      if (failMsg != null && failMsg.equals(msg)) {
        failure = new AssertionError(this.toString() + "\nGot forbidden message: " + msg);
      }
    }

    @Override
    public void flush() {}

    @Override
    public void close() throws SecurityException {}

    @Override
    public String toString() {
      var sb = new StringBuilder();
      for (var r : records) {
        sb.append("\n").append(fmt.formatMessage(r));
      }
      return sb.toString();
    }
  }
}
