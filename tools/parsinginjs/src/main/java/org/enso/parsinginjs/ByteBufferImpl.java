package org.enso.parsinginjs;

import java.nio.ByteBuffer;

public class ByteBufferImpl extends ByteBuffer {

    private final byte[] array;
    private int position;

    public static ByteBuffer wrap(byte[] array) {
        return new ByteBufferImpl(array);
    }

    public ByteBufferImpl(byte[] arr) {
        this.array = arr;
    }

    public byte get() {
        return array[position++];
    }

    public long getLong() {
        long i1 = getInt();
        long i2 = getInt();
        return i1 | (i2 << 32);
    }

    public int getInt() {
        int b1 = get();
        int b2 = get();
        int b3 = get();
        int b4 = get();
        return b1 | (b2 << 8) | (b3 << 16) | (b4 << 24);
    }

    public ByteBuffer get(byte[] dst) {
        for (int i = 0; i < dst.length; i++) {
            dst[i] = array[position++];
        }
        return this;
    }

    public final int position() {
        return position;
    }

}
