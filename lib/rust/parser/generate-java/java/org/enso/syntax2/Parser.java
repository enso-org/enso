package org.enso.syntax2;

import org.enso.syntax2.serialization.Message;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public final class Parser implements AutoCloseable {
    static {
        File parser = new File("target/rust/debug/libenso_parser.so");
        System.load(parser.getAbsolutePath());
    }

    private long state;

    private Parser(long stateIn) {
        state = stateIn;
    }
    private static native long allocState();
    private static native void freeState(long state);
    private static native ByteBuffer parseInput(long state, ByteBuffer input);
    private static native long getLastInputBase(long state);

    public static Parser create() {
        var state = allocState();
        return new Parser(state);
    }
    public final Tree parse(String input) {
        try {
            byte[] inputBytes = input.getBytes("UTF-8");
            ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
            inputBuf.put(inputBytes);
            var serializedTree = parseInput(state, inputBuf);
            var base = getLastInputBase(state);
            serializedTree.order(ByteOrder.LITTLE_ENDIAN);
            return Tree.deserialize(new Message(serializedTree, inputBuf, base));
        } catch (java.io.UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }
    public void close() {
        freeState(state);
        state = 0;
    }
}
