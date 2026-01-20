package org.enso.jvm.channel;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

final class ChannelExceptions {
  private static final int EOS = -1;
  private static final int NULL = -2;

  private ChannelExceptions() {}

  private static String readUTF(ByteBuffer buf) {
    int len = buf.getInt();
    assert len != EOS : "Unexpected end of stream in the buffer!";
    if (len == NULL) {
      return null;
    } else {
      byte[] bytes = new byte[len];
      buf.get(bytes);
      return new String(bytes, StandardCharsets.UTF_8);
    }
  }

  private static boolean putUTF(ByteBuffer buf, String txt) {
    byte[] bytes = txt == null ? null : txt.getBytes(StandardCharsets.UTF_8);
    int wholeLen = bytes == null ? Integer.BYTES : bytes.length + Integer.BYTES;
    if (wholeLen > buf.remaining()) {
      return false;
    } else {
      if (bytes == null) {
        buf.putInt(NULL);
      } else {
        buf.putInt(bytes.length);
        buf.put(bytes);
      }
      return true;
    }
  }

  static void exceptionSerialize(
      ByteBuffer buf, Throwable ex, String stopClass, String stopMethod) {
    boolean okClass = putUTF(buf, ex.getClass().getName());
    boolean okMsg = putUTF(buf, ex.getMessage());
    assert okClass && okMsg;
    int lastGood = 0;
    for (StackTraceElement elem : ex.getStackTrace()) {
      lastGood = buf.position();
      if (stopMethod.equals(elem.getMethodName()) && stopClass.equals(elem.getClassName())) {
        break;
      }
      boolean ok =
          putUTF(buf, elem.getClassName())
              && putUTF(buf, elem.getMethodName())
              && putUTF(buf, elem.getFileName());
      if (ok && buf.remaining() >= Integer.BYTES) {
        buf.putInt(elem.getLineNumber());
        if (buf.remaining() < Integer.BYTES) {
          break;
        }
      } else {
        buf.putInt(lastGood, EOS);
        return;
      }
    }
    buf.putInt(EOS);
  }

  @SuppressWarnings(value = "unchecked")
  static <E extends Throwable> E exceptionDeserialize(Class<E> type, ByteBuffer buf) throws E {
    var clazz = readUTF(buf);
    var msg = readUTF(buf);
    var stack = new ArrayList<StackTraceElement>();
    while (true) {
      int peekLen = buf.getInt(buf.position());
      if (peekLen == EOS) {
        break;
      }
      String className = readUTF(buf);
      String methodName = readUTF(buf);
      String fileName = readUTF(buf);
      int lineNo = buf.getInt();
      StackTraceElement ste = new StackTraceElement(className, methodName, fileName, lineNo);
      stack.add(ste);
    }
    var ex =
        switch (clazz) {
          case "java.lang.NullPointerException" -> new NullPointerException(msg);
          case "java.lang.IllegalArgumentException" -> new IllegalArgumentException(msg);
          case "java.lang.IllegalStateException" -> new IllegalStateException(msg);
          case "java.lang.ClassCastException" -> new ClassCastException(msg);
          default -> new IllegalStateException(clazz + ":" + msg);
        };
    stack.addAll(List.of(ex.getStackTrace()));
    ex.setStackTrace(stack.toArray(StackTraceElement[]::new));
    throw (E) ex;
  }
}
