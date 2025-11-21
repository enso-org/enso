package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.Message;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;

final class OtherInteropType {
  private static final int IS_BOOLEAN = 0x01;
  private static final int IS_STRING = 0x02;
  private static final int IS_META_OBJECT = 0x03;
  private static final int IS_EXCEPTION = 0x04;
  private static final int IS_ITERATOR = 0x05;
  private static final int IS_DURATION = 0x08;
  private static final int IS_NULL = 0x10;

  private static final int MASK_TEMPORAL = 0x0100;
  private static final int IS_DATE = MASK_TEMPORAL + 0x01;
  private static final int IS_TIME = MASK_TEMPORAL + 0x02;
  private static final int IS_ZONE = MASK_TEMPORAL + 0x04;

  private static final int MASK_NUMBER = 0x0200;
  private static final int FITS_BYTE = MASK_NUMBER + 0x01;
  private static final int FITS_SHORT = MASK_NUMBER + 0x02;
  private static final int FITS_INT = MASK_NUMBER + 0x04;
  private static final int FITS_LONG = MASK_NUMBER + 0x08;
  private static final int FITS_FLOAT = MASK_NUMBER + 0x10;
  private static final int FITS_DOUBLE = MASK_NUMBER + 0x20;
  private static final int FITS_BIG_INTEGER = MASK_NUMBER + 0x40;

  private static final int MASK_ARRAY = 0x0400;
  private static final int MASK_HASH = 0x0800;
  private static final int MASK_BUFFER = 0x1000;

  private OtherInteropType() {}

  static short findType(TruffleObject obj) {
    var iop = InteropLibrary.getUncached();
    var one = oneOf(obj, iop);
    if (iop.hasArrayElements(obj)) {
      one |= MASK_ARRAY;
    }
    if (iop.hasHashEntries(obj)) {
      one |= MASK_HASH;
    }
    if (iop.hasBufferElements(obj)) {
      one |= MASK_BUFFER;
    }
    return one;
  }

  private static short oneOf(TruffleObject obj, InteropLibrary iop) {
    if (iop.isNull(obj)) {
      return IS_NULL;
    }
    if (iop.isBoolean(obj)) {
      return IS_BOOLEAN;
    }
    if (iop.isString(obj)) {
      return IS_STRING;
    }
    if (iop.isNumber(obj)) {
      var m = MASK_NUMBER;
      if (iop.fitsInByte(obj)) {
        m |= FITS_BYTE;
      }
      if (iop.fitsInShort(obj)) {
        m |= FITS_SHORT;
      }
      if (iop.fitsInInt(obj)) {
        m |= FITS_INT;
      }
      if (iop.fitsInLong(obj)) {
        m |= FITS_LONG;
      }
      if (iop.fitsInFloat(obj)) {
        m |= FITS_FLOAT;
      }
      if (iop.fitsInDouble(obj)) {
        m |= FITS_DOUBLE;
      }
      if (iop.fitsInBigInteger(obj)) {
        m |= FITS_BIG_INTEGER;
      }
      return (short) m;
    }
    if (iop.isDuration(obj)) {
      return IS_DURATION;
    }
    if (iop.isException(obj)) {
      return IS_EXCEPTION;
    }
    if (iop.isMetaObject(obj)) {
      return IS_META_OBJECT;
    }
    if (iop.isIterator(iop)) {
      return IS_ITERATOR;
    }
    var m = 0x00;
    if (iop.isDate(obj)) {
      m |= IS_DATE;
    }
    if (iop.isTime(obj)) {
      m |= IS_TIME;
    }
    if (iop.isTimeZone(obj)) {
      m |= IS_ZONE;
    }
    return (short) m;
  }

  static boolean isNull(int v) {
    return v == IS_NULL;
  }

  static boolean isMetaObject(int v) {
    return v == IS_META_OBJECT;
  }

  static boolean isString(int v) {
    return v == IS_STRING;
  }

  static boolean isDate(int v) {
    return (v & IS_DATE) == IS_DATE;
  }

  static boolean isTime(int v) {
    return (v & IS_TIME) == IS_TIME;
  }

  static boolean isZone(int v) {
    return (v & IS_ZONE) == IS_ZONE;
  }

  static boolean isDuration(int v) {
    return v == IS_DURATION;
  }

  static boolean isNumber(int v) {
    return (v & MASK_NUMBER) == MASK_NUMBER;
  }

  static boolean fitsByte(int v) {
    return (v & FITS_BYTE) == FITS_BYTE;
  }

  static boolean fitsShort(int v) {
    return (v & FITS_SHORT) == FITS_SHORT;
  }

  static boolean fitsInt(int v) {
    return (v & FITS_INT) == FITS_INT;
  }

  static boolean fitsLong(int v) {
    return (v & FITS_LONG) == FITS_LONG;
  }

  static boolean fitsFloat(int v) {
    return (v & FITS_FLOAT) == FITS_FLOAT;
  }

  static boolean fitsDouble(int v) {
    return (v & FITS_DOUBLE) == FITS_DOUBLE;
  }

  static boolean fitsBigInteger(int v) {
    return (v & FITS_BIG_INTEGER) == FITS_BIG_INTEGER;
  }

  static boolean hasArrayElements(int v) {
    return (v & MASK_ARRAY) == MASK_ARRAY;
  }

  static boolean hasHashEntries(int v) {
    return (v & MASK_HASH) == MASK_HASH;
  }

  static boolean hasBufferElements(int v) {
    return (v & MASK_BUFFER) == MASK_BUFFER;
  }

  @Persistable(id = 1)
  static final class PersistTruffleObject extends Persistance<TruffleObject> {
    PersistTruffleObject() {
      super(TruffleObject.class, true, 1);
    }

    @Override
    protected void writeObject(TruffleObject obj, Persistance.Output out) throws IOException {
      if (obj instanceof OtherJvmObject other) {
        other.writeTo(out);
      } else {
        throw new IOException("No other subclasses of TruffleObject should get here: " + obj);
      }
    }

    @Override
    protected TruffleObject readObject(Persistance.Input in)
        throws IOException, ClassNotFoundException {
      // OtherJvmObject instance ready to be "read resolved"
      return OtherJvmObject.readFrom(in);
    }
  }

  @Persistable(id = 81902)
  static final class PersistTruffleMessage extends Persistance<Message> {
    PersistTruffleMessage() {
      super(Message.class, true, 81902);
    }

    @Override
    protected void writeObject(Message obj, Persistance.Output out) throws IOException {
      assert InteropLibrary.class == obj.getLibraryClass();
      out.writeUTF(obj.getSimpleName());
    }

    @Override
    protected Message readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var name = in.readUTF();
      return Message.resolve(InteropLibrary.class, name);
    }
  }

  @Persistable(id = 81903)
  static final class PersistObjectArray extends Persistance<Object[]> {

    PersistObjectArray() {
      super(Object[].class, true, 81903);
    }

    @Override
    protected void writeObject(Object[] obj, Persistance.Output out) throws IOException {
      var size = obj.length;
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(obj[i]);
      }
    }

    @Override
    protected Object[] readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var arr = new Object[size];
      for (var i = 0; i < size; i++) {
        arr[i] = in.readObject();
      }
      return arr;
    }
  }

  @Persistable(id = 81904)
  static final class PersistList extends Persistance<List> {
    PersistList() {
      super(List.class, true, 81904);
    }

    @Override
    protected void writeObject(List obj, Persistance.Output out) throws IOException {
      var size = obj.size();
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(obj.get(i));
      }
    }

    @Override
    protected List readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var arr = new ArrayList<Object>(size);
      while (size-- > 0) {
        arr.add(in.readObject());
      }
      return arr;
    }
  }

  //
  // primitive types
  //
  @Persistable(id = 101)
  static final class PersistBoolean extends Persistance<Boolean> {
    PersistBoolean() {
      super(Boolean.class, true, 101);
    }

    @Override
    protected void writeObject(Boolean obj, Persistance.Output out) throws IOException {
      out.writeBoolean(obj);
    }

    @Override
    protected Boolean readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      return in.readBoolean();
    }
  }

  @Persistable(id = 102)
  static final class PersistByte extends Persistance<Byte> {
    PersistByte() {
      super(Byte.class, true, 102);
    }

    @Override
    protected void writeObject(Byte obj, Persistance.Output out) throws IOException {
      out.writeByte(obj);
    }

    @Override
    protected Byte readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      return in.readByte();
    }
  }

  @Persistable(id = 103)
  static final class PersistShort extends Persistance<Short> {

    PersistShort() {
      super(Short.class, true, 103);
    }

    @Override
    protected void writeObject(Short obj, Persistance.Output out) throws IOException {
      out.writeShort(obj);
    }

    @Override
    protected Short readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      return in.readShort();
    }
  }

  @Persistable(id = 104)
  static final class PersistInteger extends Persistance<Integer> {
    PersistInteger() {
      super(Integer.class, true, 104);
    }

    @Override
    protected void writeObject(Integer obj, Persistance.Output out) throws IOException {
      out.writeInt(obj);
    }

    @Override
    protected Integer readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      return in.readInt();
    }
  }

  @Persistable(id = 105)
  static final class PersistLong extends Persistance<Long> {
    PersistLong() {
      super(Long.class, true, 105);
    }

    @Override
    protected void writeObject(Long obj, Persistance.Output out) throws IOException {
      out.writeLong(obj);
    }

    @Override
    protected Long readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      return in.readLong();
    }
  }

  @Persistable(id = 106)
  static final class PersistFloat extends Persistance<Float> {
    PersistFloat() {
      super(Float.class, true, 106);
    }

    @Override
    protected void writeObject(Float obj, Persistance.Output out) throws IOException {
      out.writeFloat(obj);
    }

    @Override
    protected Float readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      return in.readFloat();
    }
  }

  @Persistable(id = 107)
  static final class PersistDouble extends Persistance<Double> {

    PersistDouble() {
      super(Double.class, true, 107);
    }

    @Override
    protected void writeObject(Double obj, Persistance.Output out) throws IOException {
      out.writeDouble(obj);
    }

    @Override
    protected Double readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      return in.readDouble();
    }
  }

  @Persistable(id = 108)
  static final class PersistCharacter extends Persistance<Character> {
    PersistCharacter() {
      super(Character.class, true, 108);
    }

    @Override
    protected void writeObject(Character obj, Persistance.Output out) throws IOException {
      out.writeChar(obj);
    }

    @Override
    protected Character readObject(Persistance.Input in)
        throws IOException, ClassNotFoundException {
      return in.readChar();
    }
  }

  //
  // interop types
  //
  @Persistable(id = 111)
  static final class PersistString extends Persistance<String> {
    PersistString() {
      super(String.class, true, 109);
    }

    @Override
    protected void writeObject(String obj, Persistance.Output out) throws IOException {
      var bytes = obj.getBytes(StandardCharsets.UTF_8);
      out.writeInt(bytes.length);
      out.write(bytes);
    }

    @Override
    protected String readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var len = in.readInt();
      var bytes = new byte[len];
      in.readFully(bytes);
      return new String(bytes, StandardCharsets.UTF_8);
    }
  }

  @Persistable(id = 112)
  static final class PersistBigInteger extends Persistance<BigInteger> {
    PersistBigInteger() {
      super(BigInteger.class, true, 112);
    }

    @Override
    protected void writeObject(BigInteger obj, Persistance.Output out) throws IOException {
      var arr = obj.toByteArray();
      out.writeInt(arr.length);
      out.write(arr);
    }

    @Override
    protected BigInteger readObject(Persistance.Input in)
        throws IOException, ClassNotFoundException {
      var len = in.readInt();
      var arr = new byte[len];
      in.readFully(arr);
      return new BigInteger(arr);
    }
  }

  @Persistable(id = 121, clazz = ExceptionType.class)
  static final class OtherMessages {}

  @Persistable(id = 122)
  static final class PersistLocalDate extends Persistance<LocalDate> {

    public PersistLocalDate() {
      super(LocalDate.class, false, 122);
    }

    @Override
    protected void writeObject(LocalDate obj, Persistance.Output out) throws IOException {
      out.writeInt(obj.getYear());
      out.writeByte(obj.getMonthValue());
      out.writeByte(obj.getDayOfMonth());
    }

    @Override
    protected LocalDate readObject(Persistance.Input in)
        throws IOException, ClassNotFoundException {
      var year = in.readInt();
      var month = in.readByte();
      var day = in.readByte();
      return LocalDate.of(year, month, day);
    }
  }

  @Persistable(id = 123)
  static final class PersistLocalTime extends Persistance<LocalTime> {

    public PersistLocalTime() {
      super(LocalTime.class, false, 123);
    }

    @Override
    protected void writeObject(LocalTime obj, Persistance.Output out) throws IOException {
      out.writeByte(obj.getHour());
      out.writeByte(obj.getMinute());
      out.writeByte(obj.getSecond());
      out.writeInt(obj.getNano());
    }

    @Override
    protected LocalTime readObject(Persistance.Input in)
        throws IOException, ClassNotFoundException {
      var hour = in.readByte();
      var minute = in.readByte();
      var second = in.readByte();
      var nano = in.readInt();
      return LocalTime.of(hour, minute, second, nano);
    }
  }

  @Persistable(id = 124)
  static final class PersistZoneId extends Persistance<ZoneId> {

    public PersistZoneId() {
      super(ZoneId.class, true, 124);
    }

    @Override
    protected void writeObject(ZoneId obj, Persistance.Output out) throws IOException {
      out.writeUTF(obj.getId());
    }

    @Override
    protected ZoneId readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var id = in.readUTF();
      return ZoneId.of(id);
    }
  }

  @Persistable(id = 125)
  static final class PersistOptional extends Persistance<Optional> {

    public PersistOptional() {
      super(Optional.class, true, 125);
    }

    @Override
    protected void writeObject(Optional obj, Persistance.Output out) throws IOException {
      if (obj.isEmpty()) {
        out.writeBoolean(false);
      } else {
        out.writeBoolean(true);
        out.writeObject(obj.get());
      }
    }

    @Override
    protected Optional readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var is = in.readBoolean();
      if (is) {
        var obj = in.readObject();
        assert obj != null;
        return Optional.of(obj);
      } else {
        return Optional.empty();
      }
    }
  }

  @Persistable(id = 126)
  static final class PersistDuration extends Persistance<Duration> {

    public PersistDuration() {
      super(Duration.class, true, 126);
    }

    @Override
    protected void writeObject(Duration obj, Persistance.Output out) throws IOException {
      out.writeLong(obj.getSeconds());
      out.writeInt(obj.getNano());
    }

    @Override
    protected Duration readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var s = in.readLong();
      var n = in.readInt();
      return Duration.ofSeconds(s, n);
    }
  }
}
