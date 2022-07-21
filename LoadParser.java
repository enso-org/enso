package org.enso.syntax2;

import org.enso.syntax2.serialization.Message;
import java.io.File;
import java.nio.ByteBuffer;

class LoadParser {
    static {
        File parser = new File("target/rust/debug/libenso_parser.so");
        System.load(parser.getAbsolutePath());
    }
    
    private static native ByteBuffer hello(ByteBuffer buf);
    
    public static void main(String[] args) {
        System.out.println("loaded: " + LoadParser.class.getName());
        ByteBuffer buf = ByteBuffer.allocateDirect(256);
        buf.asCharBuffer().put("add x y = x + y");
        var r = hello(buf);
        Message message = new Message(r, buf, 0);
        Tree tree = Tree.deserialize(message);
        System.out.println("Native method said: " + tree);
    }
}
