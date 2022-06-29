package utils;

public final class BincodeMessage {
    private final java.nio.ByteBuffer buffer;
    private final java.nio.ByteBuffer context;
    private final long base;

    public BincodeMessage(java.nio.ByteBuffer bufferIn, java.nio.ByteBuffer contextIn, long baseIn) {
        buffer = bufferIn;
        context = contextIn;
        base = baseIn;
    }

    public long get64() {
        return buffer.getLong();
    }

    public int get32() {
        return buffer.getInt();
    }

    public boolean getBoolean() {
        return buffer.get() != 0;
    }

    public String getString() {
        int len = (int)get64();
        byte[] dst = new byte[len];
        buffer.get(dst);
        try {
            return new String(dst, "UTF-8");
        } catch (java.io.UnsupportedEncodingException e) {
            throw new utils.IncompatibleFormatException("Expected UTF-8", e);
        }
    }

    public java.nio.ByteBuffer context() {
        return context;
    }

    public int offset(int xLow32) {
        // Given the low bits of `x`, the value of `base`, and the invariant `x >= base`,
        // return `x - base`.
        long tmp = xLow32 - base;
        if (tmp < 0)
            tmp += 0x0000000100000000L;
        return (int)tmp;
    }
}
