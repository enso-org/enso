package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import java.io.IOException;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;

/** Sends a message to the other side with ReflectionLibrary-like arguments. */
@Persistable(id = 81901)
public record OtherJvmMessage(long id, Message message, List<Object> args)
    implements Function<
        Channel<OtherJvmPool>, OtherJvmResult<? extends Object, ? extends Exception>> {
  @Persistable(id = 81908, allowInlining = false)
  record ReturnValue<T, E extends Exception>(T value) implements OtherJvmResult<T, E> {
    static <T, E extends Exception> ReturnValue<T, E> create(T value) {
      return new ReturnValue<>(value);
    }
  }

  @Persistable(id = 81909, allowInlining = false)
  record ThrowValue<T, E extends Exception>(Optional<String> msg, TruffleObject exception)
      implements OtherJvmResult<T, E> {
    @Override
    @SuppressWarnings("unchecked")
    public T value() throws E {
      var ex = exception();
      var msg = msg().isPresent() ? msg().get() : null;
      assert InteropLibrary.getUncached().isException(ex);
      if (ex instanceof AbstractTruffleException truffleEx) {
        throw truffleEx;
      } else {
        throw new OtherJvmTruffleException(msg, (OtherJvmObject) ex);
      }
    }
  }

  @Persistable(id = 81910, allowInlining = false)
  record ThrowException<V, E extends Exception>(int kind, Optional<String> msg)
      implements OtherJvmResult<V, E> {
    private static final Map<Class<? extends Throwable>, Integer> kinds;

    static {
      kinds = new LinkedHashMap<>();
      kinds.put(ClassNotFoundException.class, 1);
      kinds.put(UnsupportedMessageException.class, 2);
      kinds.put(UnknownIdentifierException.class, 3);
    }

    @SuppressWarnings("unchecked")
    static <T, E extends Exception> OtherJvmResult<T, E> create(E ex) {
      var msg = Optional.ofNullable(ex.getMessage());
      if (ex instanceof OtherJvmTruffleException truffleEx) {
        var original = truffleEx.delegate;
        return new ThrowValue<>(msg, original);
      } else if (InteropLibrary.getUncached().isException(ex)
          && ex instanceof TruffleObject truffleEx) {
        return new ThrowValue<>(msg, truffleEx);
      } else {
        var kind = kinds.getOrDefault(ex.getClass(), 0);
        return new ThrowException<>(kind, msg);
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    public V value() throws E {
      var msg = msg().isPresent() ? msg().get() : null;
      switch (kind) {
        case 1 -> throw (E) new ClassNotFoundException(msg);
        case 2 -> throw (E) UnsupportedMessageException.create();
        case 3 -> throw (E) UnknownIdentifierException.create(msg);
        default -> throw new OtherJvmException(msg);
      }
    }
  }

  private static final Message IS_IDENTICAL = Message.resolve(InteropLibrary.class, "isIdentical");

  @Override
  public OtherJvmResult<? extends Object, ? extends Exception> apply(Channel<OtherJvmPool> t) {
    var node = ReflectionLibrary.getUncached();
    var prev = t.getConfig().enter(t.isMaster(), node);
    try {
      var receiver = t.getConfig().findObject(id);
      assert receiver instanceof TruffleObject;
      if (message == IS_IDENTICAL) {
        args.set(1, InteropLibrary.getUncached());
      }
      var res = node.send(receiver, message, args.toArray());
      return new ReturnValue<>(res);
    } catch (Exception ex) {
      return ThrowException.create(ex);
    } finally {
      t.getConfig().leave(t.isMaster(), node, prev);
    }
  }

  @Persistable(id = 81905)
  public record LoadClass(String name)
      implements Function<
          Channel<OtherJvmPool>, OtherJvmResult<TruffleObject, ClassNotFoundException>> {
    @Override
    public OtherJvmResult<TruffleObject, ClassNotFoundException> apply(Channel<OtherJvmPool> t) {
      assert !t.isMaster() : "Class loading only works on the slave side!";
      try {
        var clazzRaw = t.getConfig().loadClassObject(t.isMaster(), name);
        return ReturnValue.create(clazzRaw);
      } catch (ClassNotFoundException ex) {
        return ThrowException.create(ex);
      }
    }
  }

  @Persistable(id = 81906)
  public record AddToClassPath(String path) implements Function<Channel<OtherJvmPool>, Void> {
    @Override
    public Void apply(Channel<OtherJvmPool> t) {
      t.getConfig().addToClassPath(t.isMaster(), path);
      return null;
    }
  }

  @Persistable(id = 1)
  static final class PersistTruffleObject extends Persistance<TruffleObject> {
    PersistTruffleObject() {
      super(TruffleObject.class, true, 1);
    }

    @Override
    protected void writeObject(TruffleObject obj, Output out) throws IOException {
      if (obj instanceof OtherJvmObject other) {
        other.writeTo(out);
      } else {
        throw new IOException("No other subclasses of TruffleObject should get here: " + obj);
      }
    }

    @Override
    protected TruffleObject readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(Message obj, Output out) throws IOException {
      assert InteropLibrary.class == obj.getLibraryClass();
      out.writeUTF(obj.getSimpleName());
    }

    @Override
    protected Message readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(Object[] obj, Output out) throws IOException {
      var size = obj.length;
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(obj[i]);
      }
    }

    @Override
    protected Object[] readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(List obj, Output out) throws IOException {
      var size = obj.size();
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(obj.get(i));
      }
    }

    @Override
    protected List readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(Boolean obj, Output out) throws IOException {
      out.writeBoolean(obj);
    }

    @Override
    protected Boolean readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readBoolean();
    }
  }

  @Persistable(id = 102)
  static final class PersistByte extends Persistance<Byte> {
    PersistByte() {
      super(Byte.class, true, 102);
    }

    @Override
    protected void writeObject(Byte obj, Output out) throws IOException {
      out.writeByte(obj);
    }

    @Override
    protected Byte readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readByte();
    }
  }

  @Persistable(id = 103)
  static final class PersistShort extends Persistance<Short> {

    PersistShort() {
      super(Short.class, true, 103);
    }

    @Override
    protected void writeObject(Short obj, Output out) throws IOException {
      out.writeShort(obj);
    }

    @Override
    protected Short readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readShort();
    }
  }

  @Persistable(id = 104)
  static final class PersistInteger extends Persistance<Integer> {
    PersistInteger() {
      super(Integer.class, true, 104);
    }

    @Override
    protected void writeObject(Integer obj, Output out) throws IOException {
      out.writeInt(obj);
    }

    @Override
    protected Integer readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readInt();
    }
  }

  @Persistable(id = 105)
  static final class PersistLong extends Persistance<Long> {
    PersistLong() {
      super(Long.class, true, 105);
    }

    @Override
    protected void writeObject(Long obj, Output out) throws IOException {
      out.writeLong(obj);
    }

    @Override
    protected Long readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readLong();
    }
  }

  @Persistable(id = 106)
  static final class PersistFloat extends Persistance<Float> {
    PersistFloat() {
      super(Float.class, true, 106);
    }

    @Override
    protected void writeObject(Float obj, Output out) throws IOException {
      out.writeFloat(obj);
    }

    @Override
    protected Float readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readFloat();
    }
  }

  @Persistable(id = 107)
  static final class PersistDouble extends Persistance<Double> {

    PersistDouble() {
      super(Double.class, true, 107);
    }

    @Override
    protected void writeObject(Double obj, Output out) throws IOException {
      out.writeDouble(obj);
    }

    @Override
    protected Double readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readDouble();
    }
  }

  @Persistable(id = 108)
  static final class PersistCharacter extends Persistance<Character> {
    PersistCharacter() {
      super(Character.class, true, 108);
    }

    @Override
    protected void writeObject(Character obj, Output out) throws IOException {
      out.writeChar(obj);
    }

    @Override
    protected Character readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(String obj, Output out) throws IOException {
      out.writeUTF(obj);
    }

    @Override
    protected String readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readUTF();
    }
  }

  @Persistable(id = 112)
  static final class PersistBigInteger extends Persistance<BigInteger> {
    PersistBigInteger() {
      super(BigInteger.class, true, 112);
    }

    @Override
    protected void writeObject(BigInteger obj, Output out) throws IOException {
      var arr = obj.toByteArray();
      out.writeInt(arr.length);
      out.write(arr);
    }

    @Override
    protected BigInteger readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(LocalDate obj, Output out) throws IOException {
      out.writeInt(obj.getYear());
      out.writeByte(obj.getMonthValue());
      out.writeByte(obj.getDayOfMonth());
    }

    @Override
    protected LocalDate readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(LocalTime obj, Output out) throws IOException {
      out.writeByte(obj.getHour());
      out.writeByte(obj.getMinute());
      out.writeByte(obj.getSecond());
      out.writeInt(obj.getNano());
    }

    @Override
    protected LocalTime readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(ZoneId obj, Output out) throws IOException {
      out.writeUTF(obj.getId());
    }

    @Override
    protected ZoneId readObject(Input in) throws IOException, ClassNotFoundException {
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
    protected void writeObject(Optional obj, Output out) throws IOException {
      if (obj.isEmpty()) {
        out.writeBoolean(false);
      } else {
        out.writeBoolean(true);
        out.writeObject(obj.get());
      }
    }

    @Override
    protected Optional readObject(Input in) throws IOException, ClassNotFoundException {
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
}
