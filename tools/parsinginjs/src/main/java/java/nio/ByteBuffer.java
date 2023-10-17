package java.nio;

import org.enso.parsinginjs.ByteBufferImpl;

public abstract class ByteBuffer {

    public static ByteBuffer wrap(byte[] array) {
        return new ByteBufferImpl(array);
    }

    protected ByteBuffer() {
    }

    public abstract byte get();

    public abstract long getLong();

    public abstract int getInt();

    public abstract ByteBuffer get(byte[] dst);

    public abstract int position();
}
